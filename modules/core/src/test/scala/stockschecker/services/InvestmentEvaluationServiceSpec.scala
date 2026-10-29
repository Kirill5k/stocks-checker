package stockschecker.services

import cats.effect.{IO, Ref}
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.domain.*
import stockschecker.domain.investment.{EvaluationCapture, InvestmentEvaluator}
import stockschecker.fixtures.AAPL
import stockschecker.repositories.*

import java.time.{Instant, LocalDate, YearMonth, ZoneOffset}

class InvestmentEvaluationServiceSpec extends IOWordSpec {
  private val now          = Instant.parse("2026-10-19T12:00:00Z")
  private val currentMonth = YearMonth.of(2026, 10)
  given Logger[IO]         = Slf4jLogger.getLogger[IO]
  given Clock[IO]          = Clock.mock(now)

  private def screen(id: String = "screen", formed: String = "2026-09-01"): InvestmentScreen =
    val createdAt = LocalDate.parse(formed).atStartOfDay(ZoneOffset.UTC).toInstant
    InvestmentScreen(
      InvestmentScreenId(id),
      createdAt,
      InvestmentRankingId("ranking"),
      InvestmentScreenRequest(),
      List(InvestmentCohort(CohortName.Investment, List(AAPL))),
      InvestmentScreenSummary(InvestmentScreenId(id), createdAt, InvestmentRecipe().version, 1, 1, 1, Map.empty)
    )

  private def snapshots(
      screen: InvestmentScreen,
      month: YearMonth = currentMonth,
      audited: Boolean = true
  ): Map[Ticker, PriceHistorySnapshot] =
    val from    = screen.createdAt.atZone(ZoneOffset.UTC).toLocalDate
    val through = InvestmentEvaluation.captureThrough(from, month)
    List(AAPL, InvestmentEvaluator.Benchmark).map { ticker =>
      val candles = List(from.plusDays(1) -> BigDecimal(100), through -> BigDecimal(105)).map { case (date, close) =>
        PriceCandle(date, close, close, close, close, 1L)
      }
      ticker -> PriceHistorySnapshot(
        ticker.value,
        ticker,
        candles,
        Provider.TwelveData,
        if audited then "USD" else "EUR",
        PriceAdjustment.SplitsAndDividends,
        now,
        DateRange(from, through)
      )
    }.toMap

  private case class Harness(
      service: InvestmentEvaluationService[IO],
      documents: Ref[IO, Map[(InvestmentScreenId, YearMonth), InvestmentEvaluation]],
      jobs: Ref[IO, Map[JobId, Boolean]],
      cache: Ref[IO, Map[Ticker, PriceHistorySnapshot]],
      writes: Ref[IO, List[InvestmentEvaluation]],
      dispatcher: ActionDispatcher[IO],
      repository: InvestmentEvaluationRepository[IO],
      requests: Ref[IO, Map[(InvestmentScreenId, YearMonth), Set[Ticker]]]
  ) {
    def persist(report: InvestmentEvaluation): IO[Unit] =
      writes.update(_ :+ report) >> documents.update { saved =>
        val key = report.screenId -> report.captureMonth
        if saved.get(key).exists(_.settled) then saved else saved.updated(key, report)
      }
  }

  private def harness(screens: List[InvestmentScreen], saved: List[InvestmentEvaluation] = Nil): IO[Harness] =
    for
      documents <- Ref.of[IO, Map[(InvestmentScreenId, YearMonth), InvestmentEvaluation]](
        saved.map(e => (e.screenId -> e.captureMonth) -> e).toMap
      )
      requests <- Ref.of[IO, Map[(InvestmentScreenId, YearMonth), Set[Ticker]]](Map.empty)
      jobs     <- Ref.of[IO, Map[JobId, Boolean]](Map.empty)
      cache    <- Ref.of[IO, Map[Ticker, PriceHistorySnapshot]](Map.empty)
      writes   <- Ref.of[IO, List[InvestmentEvaluation]](Nil)
      dispatcher = mock[ActionDispatcher[IO]]
      repository = mock[InvestmentEvaluationRepository[IO]]
      _          = when(repository.findContext(any[InvestmentScreenId], any[YearMonth])).thenAnswer { i =>
        val id    = i.getArgument[InvestmentScreenId](0)
        val month = i.getArgument[YearMonth](1)
        documents.get.map { saved =>
          screens.find(_.id == id).map { screen =>
            val monthly  = saved.get(id -> month)
            val finished = saved.values.filter(e => e.screenId == id && e.isFinished).toList.sortBy(_.captureMonth).lastOption
            EvaluationCapture.Context(screen, monthly, finished)
          }
        }
      }
      _ = when(repository.recordHistoryRequests(any[InvestmentScreenId], any[YearMonth], any[List[Ticker]])).thenAnswer { i =>
        val key     = i.getArgument[InvestmentScreenId](0) -> i.getArgument[YearMonth](1)
        val tickers = i.getArgument[List[Ticker]](2)
        requests.update(saved => saved.updated(key, saved.getOrElse(key, Set.empty) ++ tickers))
      }
      _ = when(repository.captureTasks(any[YearMonth])).thenAnswer { i =>
        val month = i.getArgument[YearMonth](0)
        for
          saved    <- documents.get
          recorded <- requests.get
        yield screens.flatMap { screen =>
          val reports = saved.values.filter(_.screenId == screen.id).toList
          val months  = (Option.when(!reports.exists(_.isFinished))(month).toList ++
            reports.filterNot(_.settled).map(_.captureMonth)).distinct
          months.filterNot(m => saved.get(screen.id -> m).exists(_.settled)).map { m =>
            val existing =
              saved.get(screen.id -> m).map(_ => EvaluationCapture.State(recorded.getOrElse(screen.id -> m, Set.empty)))
            EvaluationCapture.Task(screen, m, existing)
          }
        }
      }
      _ = when(repository.findPriceSeries(any[List[Ticker]], any[DateRange])).thenAnswer { i =>
        val tickers = i.getArgument[List[Ticker]](0).toSet
        val range   = i.getArgument[DateRange](1)
        cache.get.map(_.collect { case (ticker, snapshot) if tickers(ticker) => ticker -> EvaluationPriceSeries.from(snapshot, range) })
      }
      _ = when(dispatcher.dispatchOnce(any[JobId], any[Action])).thenAnswer { i =>
        val id = i.getArgument[JobId](0)
        jobs.update(values => values.updated(id, values.getOrElse(id, true)))
      }
      _ = when(dispatcher.pending(any[List[JobId]])).thenAnswer { i =>
        val ids = i.getArgument[List[JobId]](0).toSet
        jobs.get.map(_.collect { case (id, true) if ids(id) => id }.toSet)
      }
      service <- InvestmentEvaluationService.make(repository, dispatcher)
      result = Harness(service, documents, jobs, cache, writes, dispatcher, repository, requests)
      _ = when(repository.saveMonthly(any[InvestmentEvaluation])).thenAnswer(i => result.persist(i.getArgument[InvestmentEvaluation](0)))
    yield result

  "InvestmentEvaluationService" should {
    "return pending horizons for a newly formed screen without querying future prices" in {
      val current = screen(formed = "2026-10-19")
      (for
        h      <- harness(List(current))
        report <- h.service.evaluate(current.id)
        writes <- h.writes.get
      yield {
        report.horizons.map(_.status) mustBe List(EvaluationStatus.Pending, EvaluationStatus.Pending)
        report.captureMonth mustBe currentMonth
        writes mustBe Nil
        verifyNoInteractions(h.dispatcher)
        verify(h.repository, never).findPriceSeries(any[List[Ticker]], any[DateRange])
        succeed
      }).asserting(identity)
    }

    "read cached reports without dispatching jobs or writing captures" in {
      val current = screen()
      (for
        h      <- harness(List(current))
        report <- h.service.evaluate(current.id)
        writes <- h.writes.get
      yield {
        report.settled mustBe false
        report.dataThrough mustBe LocalDate.parse("2026-09-30")
        report.pendingDataJobs mustBe Nil
        writes mustBe Nil
        verify(h.dispatcher, never).dispatchOnce(any[JobId], any[Action])
        succeed
      }).asserting(identity)
    }

    "capture other screens when one screen fails" in {
      val broken  = screen("broken")
      val healthy = screen("healthy")
      val failure = new IllegalStateException("screen read failed")
      (for
        h <- harness(List(broken, healthy))
        _ <- h.cache.set(snapshots(healthy))
        _ = when(h.repository.saveMonthly(any[InvestmentEvaluation])).thenAnswer { i =>
          val report = i.getArgument[InvestmentEvaluation](0)
          if report.screenId == broken.id then IO.raiseError(failure) else h.persist(report)
        }
        count <- h.service.captureMonthlyEvaluations
        saved <- h.documents.get
      yield {
        count mustBe 1
        saved.keySet mustBe Set(healthy.id -> currentMonth)
        saved(healthy.id -> currentMonth).settled mustBe true
        succeed
      }).asserting(identity)
    }

    "attempt every screen and fail when all captures fail" in {
      val first   = screen("first")
      val second  = screen("second")
      val failure = new IllegalStateException("capture unavailable")
      (for
        h <- harness(List(first, second))
        _ = when(h.repository.saveMonthly(any[InvestmentEvaluation])).thenReturn(IO.raiseError(failure))
        result <- h.service.captureMonthlyEvaluations.attempt
      yield {
        result mustBe Left(failure)
        verify(h.repository, times(2)).saveMonthly(any[InvestmentEvaluation])
        succeed
      }).asserting(identity)
    }

    "succeed when there are no captures to run" in
      harness(Nil).flatMap(_.service.captureMonthlyEvaluations).asserting(_ mustBe 0)

    "persist pending captures, resume them, and freeze each settled month" in {
      val current = screen()
      (for
        h       <- harness(List(current))
        first   <- h.service.captureMonthlyEvaluations
        pending <- h.documents.get.map(_(current.id -> currentMonth))
        _       <- h.cache.set(snapshots(current))
        _       <- h.jobs.update(_.view.mapValues(_ => false).toMap)
        second  <- h.service.captureMonthlyEvaluations
        settled <- h.service.evaluate(current.id)
        third   <- h.service.captureMonthlyEvaluations
        frozen  <- h.service.evaluate(current.id)
      yield {
        first mustBe 0
        pending.settled mustBe false
        pending.pendingDataJobs.size mustBe 2
        second mustBe 1
        settled.settled mustBe true
        settled.isComplete mustBe true
        third mustBe 0
        frozen mustBe settled
        succeed
      }).asserting(identity)
    }

    "resume a prior month's document using its original month cutoff" in {
      val current  = screen(formed = "2026-08-01")
      val previous = YearMonth.of(2026, 9)
      val through  = InvestmentEvaluation.captureThrough(LocalDate.parse("2026-08-01"), previous)
      val saved    = InvestmentEvaluator
        .evaluate(current.id, current.createdAt, now, current.cohorts, Map.empty, previous, through)
      (for
        h <- harness(List(current), List(saved))
        _ = when(h.repository.captureTasks(currentMonth))
          .thenReturnIO(List(EvaluationCapture.Task(current, previous, Some(EvaluationCapture.State(Set.empty)))))
        _         <- h.cache.set(snapshots(current, previous))
        count     <- h.service.captureMonthlyEvaluations
        documents <- h.documents.get
      yield {
        count mustBe 1
        documents.keySet mustBe Set(current.id -> previous)
        documents(current.id -> previous).dataThrough mustBe LocalDate.parse("2026-08-31")
        documents(current.id -> previous).captureMonth mustBe previous
        documents(current.id -> previous).settled mustBe true
        verifyNoInteractions(h.dispatcher)
        succeed
      }).asserting(identity)
    }

    "recover prior and current months together with separate cutoff jobs" in {
      val current        = screen(formed = "2026-08-01")
      val previous       = YearMonth.of(2026, 9)
      val priorThrough   = LocalDate.parse("2026-08-31")
      val currentThrough = LocalDate.parse("2026-09-30")
      val saved          = InvestmentEvaluator
        .evaluate(current.id, current.createdAt, now, current.cohorts, Map.empty, previous, priorThrough)
      val tickers     = Set(AAPL, InvestmentEvaluator.Benchmark)
      val priorJobs   = tickers.map(InvestmentEvaluationService.historyJobId(_, priorThrough))
      val currentJobs = tickers.map(InvestmentEvaluationService.historyJobId(_, currentThrough))
      (for
        h         <- harness(List(current), List(saved))
        count     <- h.service.captureMonthlyEvaluations
        documents <- h.documents.get
        jobs      <- h.jobs.get
      yield {
        count mustBe 0
        documents.keySet mustBe Set(current.id -> previous, current.id -> currentMonth)
        documents(current.id -> previous).dataThrough mustBe priorThrough
        documents(current.id -> currentMonth).dataThrough mustBe currentThrough
        documents(current.id -> previous).pendingDataJobs.toSet mustBe priorJobs
        documents(current.id -> currentMonth).pendingDataJobs.toSet mustBe currentJobs
        priorJobs.intersect(currentJobs) mustBe empty
        jobs.keySet mustBe priorJobs ++ currentJobs
        succeed
      }).asserting(identity)
    }

    "keep the pending document when the final capture write fails" in {
      val current = screen()
      val failure = new IllegalStateException("final save failed")
      (for
        h <- harness(List(current))
        _ <- h.cache.set(snapshots(current))
        _ = when(h.repository.saveMonthly(any[InvestmentEvaluation])).thenAnswer { i =>
          val report = i.getArgument[InvestmentEvaluation](0)
          if report.settled then IO.raiseError(failure) else h.persist(report)
        }
        result    <- h.service.captureMonthlyEvaluations.attempt
        documents <- h.documents.get
      yield {
        result mustBe Left(failure)
        documents(current.id -> currentMonth).settled mustBe false
        succeed
      }).asserting(identity)
    }

    "avoid dispatch when the initial capture cannot be persisted" in {
      val failure = new IllegalStateException("initial save failed")
      (for
        h <- harness(List(screen()))
        _ = when(h.repository.saveMonthly(any[InvestmentEvaluation])).thenReturn(IO.raiseError(failure))
        result <- h.service.captureMonthlyEvaluations.attempt
      yield {
        result mustBe Left(failure)
        verify(h.dispatcher, never).dispatchOnce(any[JobId], any[Action])
        succeed
      }).asserting(identity)
    }

    "share deterministic price jobs across screens formed on different dates" in {
      val screens = List(screen("first", "2026-08-01"), screen("second", "2026-09-01"))
      (for
        h         <- harness(screens)
        count     <- h.service.captureMonthlyEvaluations
        jobs      <- h.jobs.get
        documents <- h.documents.get
      yield {
        count mustBe 0
        jobs.size mustBe 2
        documents.size mustBe 2
        documents.values.map(_.pendingDataJobs.toSet).toSet.size mustBe 1
        verify(h.repository).captureTasks(currentMonth)
        verify(h.repository, never).findContext(any[InvestmentScreenId], any[YearMonth])
        succeed
      }).asserting(identity)
    }

    List(false, true).foreach { unavailable =>
      s"settle incomplete data without restarting terminal jobs (unavailable=$unavailable)" in {
        val current = screen()
        val through = LocalDate.parse("2026-09-30")
        (for
          h <- harness(List(current))
          _ <-
            if unavailable then
              h.jobs.set(
                List(AAPL, InvestmentEvaluator.Benchmark).map(t => InvestmentEvaluationService.historyJobId(t, through) -> false).toMap
              )
            else h.cache.set(snapshots(current, audited = false))
          first  <- h.service.captureMonthlyEvaluations
          second <- h.service.captureMonthlyEvaluations
          report <- h.service.evaluate(current.id)
        yield {
          first mustBe 1
          second mustBe 0
          report.settled mustBe true
          report.isComplete mustBe false
          report.pendingDataJobs mustBe Nil
          if unavailable then report.horizons.flatMap(_.issues).mkString must include("remains unavailable after completed capture jobs")
          else verifyNoInteractions(h.dispatcher)
          succeed
        }).asserting(identity)
      }
    }

    "include prices from a worker that finishes between dispatch and the pending query" in {
      val current = screen()
      (for
        h <- harness(List(current))
        _ = when(h.dispatcher.dispatchOnce(any[JobId], any[Action])).thenReturn(h.cache.set(snapshots(current)))
        count  <- h.service.captureMonthlyEvaluations
        report <- h.service.evaluate(current.id)
      yield {
        count mustBe 1
        report.isComplete mustBe true
        report.priceEvidence.size mustBe 2
        succeed
      }).asserting(identity)
    }

    "remember a pruned terminal request while its sibling remains pending" in {
      val current      = screen()
      val through      = LocalDate.parse("2026-09-30")
      val cohortJob    = InvestmentEvaluationService.historyJobId(AAPL, through)
      val benchmarkJob = InvestmentEvaluationService.historyJobId(InvestmentEvaluator.Benchmark, through)
      (for
        h         <- harness(List(current))
        first     <- h.service.captureMonthlyEvaluations
        _         <- h.jobs.update(_ - cohortJob)
        second    <- h.service.captureMonthlyEvaluations
        pending   <- h.documents.get.map(_(current.id -> currentMonth))
        requested <- h.requests.get
        jobs      <- h.jobs.get
        _         <- h.jobs.set(Map.empty)
        third     <- h.service.captureMonthlyEvaluations
        settled   <- h.service.evaluate(current.id)
      yield {
        first mustBe 0
        second mustBe 0
        pending.pendingDataJobs mustBe List(benchmarkJob)
        requested(current.id -> currentMonth) mustBe Set(AAPL, InvestmentEvaluator.Benchmark)
        jobs.keySet mustBe Set(benchmarkJob)
        third mustBe 1
        settled.settled mustBe true
        settled.isComplete mustBe false
        verify(h.dispatcher, times(2)).dispatchOnce(any[JobId], any[Action])
        succeed
      }).asserting(identity)
    }

    "record successful requests before recovering a later failed dispatch" in {
      val current      = screen()
      val through      = LocalDate.parse("2026-09-30")
      val cohortJob    = InvestmentEvaluationService.historyJobId(AAPL, through)
      val benchmarkJob = InvestmentEvaluationService.historyJobId(InvestmentEvaluator.Benchmark, through)
      val cohortAction = Action.RefreshEvaluationPrices(AAPL, through)
      val failure      = new IllegalStateException("second dispatch failed")
      (for
        h <- harness(List(current))
        _ = when(h.dispatcher.dispatchOnce(cohortJob, cohortAction)).thenReturn(IO.raiseError(failure))
        failed   <- h.service.captureMonthlyEvaluations.attempt
        recorded <- h.requests.get
        _        <- h.jobs.set(Map.empty)
        _ = when(h.dispatcher.dispatchOnce(cohortJob, cohortAction)).thenReturn(h.jobs.update(_ + (cohortJob -> true)))
        resumed   <- h.service.captureMonthlyEvaluations
        jobs      <- h.jobs.get
        requested <- h.requests.get
      yield {
        failed mustBe Left(failure)
        recorded(current.id -> currentMonth) mustBe Set(InvestmentEvaluator.Benchmark)
        resumed mustBe 0
        jobs.keySet mustBe Set(cohortJob)
        requested(current.id -> currentMonth) mustBe Set(AAPL, InvestmentEvaluator.Benchmark)
        verify(h.dispatcher).dispatchOnce(
          benchmarkJob,
          Action.RefreshEvaluationPrices(InvestmentEvaluator.Benchmark, through)
        )
        verify(h.dispatcher, times(2)).dispatchOnce(cohortJob, cohortAction)
        succeed
      }).asserting(identity)
    }

    "retain dispatched requests when the final capture write fails" in {
      val current = screen()
      val failure = new IllegalStateException("settled save failed")
      (for
        h             <- harness(List(current))
        failFinalSave <- Ref.of[IO, Boolean](true)
        _ = when(h.dispatcher.dispatchOnce(any[JobId], any[Action])).thenReturn(IO.unit)
        _ = when(h.repository.saveMonthly(any[InvestmentEvaluation])).thenAnswer { i =>
          val report = i.getArgument[InvestmentEvaluation](0)
          failFinalSave.get.flatMap(fail => if fail && report.settled then IO.raiseError(failure) else h.persist(report))
        }
        failed   <- h.service.captureMonthlyEvaluations.attempt
        recorded <- h.requests.get
        _        <- failFinalSave.set(false)
        resumed  <- h.service.captureMonthlyEvaluations
      yield {
        failed mustBe Left(failure)
        recorded(current.id -> currentMonth) mustBe Set(AAPL, InvestmentEvaluator.Benchmark)
        resumed mustBe 1
        verify(h.dispatcher, times(2)).dispatchOnce(any[JobId], any[Action])
        succeed
      }).asserting(identity)
    }

    "stop monthly captures after the final horizon and return the saved final report" in {
      val current  = screen(formed = "2020-01-01")
      val previous = YearMonth.of(2025, 2)
      val through  = InvestmentEvaluation.captureThrough(LocalDate.parse("2020-01-01"), previous)
      val report   = InvestmentEvaluator
        .evaluate(current.id, current.createdAt, now, current.cohorts, Map.empty, previous, through)
        .copy(settled = true)
      (for
        h      <- harness(List(current), List(report))
        count  <- h.service.captureMonthlyEvaluations
        result <- h.service.evaluate(current.id)
      yield {
        report.isFinished mustBe true
        count mustBe 0
        result mustBe report
        verifyNoInteractions(h.dispatcher)
        verify(h.repository, never).findPriceSeries(any[List[Ticker]], any[DateRange])
        verify(h.repository, never).saveMonthly(any[InvestmentEvaluation])
        succeed
      }).asserting(identity)
    }
  }
}
