package stockschecker.services

import cats.effect.{IO, Ref}
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.domain.*
import stockschecker.domain.investment.InvestmentEvaluator
import stockschecker.fixtures.AAPL
import stockschecker.repositories.*

import java.time.{Instant, LocalDate, YearMonth, ZoneOffset}

class InvestmentEvaluationServiceSpec extends IOWordSpec {
  private val now          = Instant.parse("2026-10-19T12:00:00Z")
  private val currentMonth = YearMonth.of(2026, 10)
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
      prices: PriceHistoryService[IO],
      dispatcher: ActionDispatcher[IO],
      repository: InvestmentEvaluationRepository[IO],
      screens: InvestmentScreenRepository[IO]
  ) {
    def persist(report: InvestmentEvaluation): IO[Unit] =
      writes.update(_ :+ report) >> documents.update { saved =>
        val key = report.screenId -> report.captureMonth.get
        if saved.get(key).exists(_.settled) then saved else saved.updated(key, report)
      }
  }

  private def harness(screens: List[InvestmentScreen], saved: List[InvestmentEvaluation] = Nil): IO[Harness] =
    for
      documents <- Ref.of[IO, Map[(InvestmentScreenId, YearMonth), InvestmentEvaluation]](
        saved.map(e => (e.screenId -> e.captureMonth.get) -> e).toMap
      )
      jobs   <- Ref.of[IO, Map[JobId, Boolean]](Map.empty)
      cache  <- Ref.of[IO, Map[Ticker, PriceHistorySnapshot]](Map.empty)
      writes <- Ref.of[IO, List[InvestmentEvaluation]](Nil)
      prices     = mock[PriceHistoryService[IO]]
      dispatcher = mock[ActionDispatcher[IO]]
      repository = mock[InvestmentEvaluationRepository[IO]]
      screenRepo = mock[InvestmentScreenRepository[IO]]
      _          = when(screenRepo.ids).thenReturnIO(screens.map(_.id))
      _          = when(screenRepo.find(any[InvestmentScreenId])).thenAnswer(i =>
        IO.pure(screens.find(_.id == i.getArgument[InvestmentScreenId](0)).get)
      )
      _ = when(repository.findMonthly(any[InvestmentScreenId], any[YearMonth])).thenAnswer(i =>
        documents.get.map(_.get(i.getArgument[InvestmentScreenId](0) -> i.getArgument[YearMonth](1)))
      )
      _ = when(repository.unsettled).thenReturn(documents.get.map(_.values.filterNot(_.settled).toList))
      _ = when(repository.finishedScreenIds).thenReturn(documents.get.map(_.values.filter(_.isFinished).map(_.screenId).toSet))
      _ = when(repository.findFinished(any[InvestmentScreenId])).thenAnswer(i =>
        documents.get.map(
          _.values.filter(e => e.screenId == i.getArgument[InvestmentScreenId](0) && e.isFinished).toList.sortBy(_.captureMonth).lastOption
        )
      )
      _ = when(prices.cachedHistorical(any[Ticker], any[DateRange])).thenAnswer(i => cache.get.map(_.get(i.getArgument[Ticker](0))))
      _ = when(dispatcher.dispatchOnce(any[JobId], any[Action])).thenAnswer { i =>
        val id = i.getArgument[JobId](0)
        jobs.update(values => values.updated(id, values.getOrElse(id, true)))
      }
      _ = when(dispatcher.pending(any[List[JobId]])).thenAnswer { i =>
        val ids = i.getArgument[List[JobId]](0).toSet
        jobs.get.map(_.collect { case (id, true) if ids(id) => id }.toSet)
      }
      service <- InvestmentEvaluationService.make(screenRepo, repository, prices, dispatcher)
      result = Harness(service, documents, jobs, cache, writes, prices, dispatcher, repository, screenRepo)
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
        writes mustBe Nil
        verifyNoInteractions(h.prices, h.dispatcher)
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
        report.dataThrough mustBe Some(LocalDate.parse("2026-09-30"))
        report.pendingDataJobs mustBe Nil
        writes mustBe Nil
        verify(h.dispatcher, never).dispatchOnce(any[JobId], any[Action])
        verify(h.prices, never).historical(any[Ticker], any[DateRange])
        succeed
      }).asserting(identity)
    }

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
        verify(h.prices, never).historical(any[Ticker], any[DateRange])
        succeed
      }).asserting(identity)
    }

    "resume a prior month's document using its original month cutoff" in {
      val current  = screen(formed = "2026-08-01")
      val previous = YearMonth.of(2026, 9)
      val through  = InvestmentEvaluation.captureThrough(LocalDate.parse("2026-08-01"), previous)
      val saved    = InvestmentEvaluator
        .evaluate(current.id, current.createdAt, now, current.cohorts, Map.empty, Some(through))
        .copy(captureMonth = Some(previous))
      (for
        h <- harness(List(current), List(saved))
        _ = when(h.screens.ids).thenReturnIO(Nil)
        _         <- h.cache.set(snapshots(current, previous))
        count     <- h.service.captureMonthlyEvaluations
        documents <- h.documents.get
      yield {
        count mustBe 1
        documents.keySet mustBe Set(current.id -> previous)
        documents(current.id -> previous).dataThrough mustBe Some(LocalDate.parse("2026-08-31"))
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
        .evaluate(current.id, current.createdAt, now, current.cohorts, Map.empty, Some(priorThrough))
        .copy(captureMonth = Some(previous))
      val tickers     = Set(AAPL, InvestmentEvaluator.Benchmark)
      val priorJobs   = tickers.map(InvestmentEvaluationService.historyJobId(_, priorThrough))
      val currentJobs = tickers.map(InvestmentEvaluationService.historyJobId(_, currentThrough))
      (for
        h            <- harness(List(current), List(saved))
        count        <- h.service.captureMonthlyEvaluations
        documents    <- h.documents.get
        jobs         <- h.jobs.get
        protectedIds <- h.service.protectedJobIds
      yield {
        count mustBe 0
        documents.keySet mustBe Set(current.id -> previous, current.id -> currentMonth)
        documents(current.id -> previous).dataThrough mustBe Some(priorThrough)
        documents(current.id -> currentMonth).dataThrough mustBe Some(currentThrough)
        documents(current.id -> previous).pendingDataJobs.toSet mustBe priorJobs
        documents(current.id -> currentMonth).pendingDataJobs.toSet mustBe currentJobs
        priorJobs.intersect(currentJobs) mustBe empty
        jobs.keySet mustBe priorJobs ++ currentJobs
        protectedIds mustBe jobs.keySet
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

    "protect terminal cohort jobs as well as pending benchmark jobs until their saved month settles" in {
      val current      = screen(formed = "2026-08-01")
      val previous     = YearMonth.of(2026, 9)
      val through      = LocalDate.parse("2026-08-31")
      val cohortJob    = InvestmentEvaluationService.historyJobId(AAPL, through)
      val benchmarkJob = InvestmentEvaluationService.historyJobId(InvestmentEvaluator.Benchmark, through)
      val report       = InvestmentEvaluator
        .evaluate(current.id, current.createdAt, now, current.cohorts, Map.empty, Some(through))
        .copy(captureMonth = Some(previous), pendingDataJobs = List(benchmarkJob))
      (for
        h            <- harness(List(current), List(report))
        _            <- h.jobs.set(Map(cohortJob -> false, benchmarkJob -> true))
        protectedIds <- h.service.protectedJobIds
        _            <- h.persist(report.copy(settled = true, pendingDataJobs = Nil))
        released     <- h.service.protectedJobIds
      yield {
        protectedIds mustBe Set(cohortJob, benchmarkJob)
        released mustBe empty
        verifyNoInteractions(h.prices, h.dispatcher)
        succeed
      }).asserting(identity)
    }

    "stop monthly captures after the final horizon and return the saved final report" in {
      val current  = screen(formed = "2020-01-01")
      val previous = YearMonth.of(2025, 2)
      val through  = InvestmentEvaluation.captureThrough(LocalDate.parse("2020-01-01"), previous)
      val report   = InvestmentEvaluator
        .evaluate(current.id, current.createdAt, now, current.cohorts, Map.empty, Some(through))
        .copy(captureMonth = Some(previous), settled = true)
      (for
        h      <- harness(List(current), List(report))
        count  <- h.service.captureMonthlyEvaluations
        result <- h.service.evaluate(current.id)
      yield {
        report.isFinished mustBe true
        count mustBe 0
        result mustBe report
        verifyNoInteractions(h.prices, h.dispatcher)
        verify(h.repository, never).saveMonthly(any[InvestmentEvaluation])
        succeed
      }).asserting(identity)
    }
  }
}
