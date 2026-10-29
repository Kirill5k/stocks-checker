package stockschecker.repositories

import cats.effect.IO
import cats.syntax.parallel.*
import cats.syntax.foldable.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.operations.Filter
import stockschecker.domain.*
import stockschecker.domain.investment.{EvaluationCapture, InvestmentEvaluator}
import stockschecker.fixtures.*

import java.time.{Instant, LocalDate, YearMonth, ZoneOffset}
import stockschecker.repositories.InvestmentRepositoryFixtures.*
import stockschecker.repositories.entities.InvestmentEvaluationEntity

class InvestmentEvaluationRepositorySpec extends RepositorySpec with MongoJsonCodecs:
  override val port = 12221

  private val month    = YearMonth.of(2026, 10)
  private val formedAt = Instant.parse("2026-08-01T00:00:00Z")
  private val current  = screen.copy(createdAt = formedAt)

  private def report(screen: InvestmentScreen = current, captureMonth: YearMonth = month): InvestmentEvaluation =
    val from = LocalDate.ofInstant(screen.createdAt, ZoneOffset.UTC)
    InvestmentEvaluator.evaluate(
      screen.id,
      screen.createdAt,
      now,
      screen.cohorts,
      Map.empty,
      captureMonth,
      InvestmentEvaluation.captureThrough(from, captureMonth)
    )

  private def snapshot(
      ticker: Ticker,
      range: DateRange,
      fetchedAt: Instant = now,
      adjustment: PriceAdjustment = PriceAdjustment.SplitsAndDividends
  ): PriceHistorySnapshot =
    PriceHistorySnapshot(
      s"provider-${ticker.value}",
      ticker,
      List(range.from, range.through).distinct.map(date => PriceCandle(date, 100, 100, 100, 100, 1L)),
      Provider.TwelveData,
      "USD",
      adjustment,
      fetchedAt,
      range
    )

  "An InvestmentEvaluationRepository" should {
    "read a screen with absent captures and distinguish an absent screen" in
      withEmbeddedMongoDatabase { db =>
        for
          screens <- InvestmentScreenRepository.make[IO](db)
          repo    <- InvestmentEvaluationRepository.make[IO](db)
          absent  <- repo.findContext(current.id, month)
          empty   <- repo.captureTasks(month)
          _       <- screens.save(current)
          context <- repo.findContext(current.id, month)
          tasks   <- repo.captureTasks(month)
        yield
          absent mustBe None
          empty mustBe Nil
          context mustBe Some(EvaluationCapture.Context(current, None, None))
          tasks mustBe List(EvaluationCapture.Task(current, month, None))
      }

    "join the requested monthly report and the latest finished report for only that screen" in
      withEmbeddedMongoDatabase { db =>
        val old       = current.copy(createdAt = Instant.parse("2020-01-01T00:00:00Z"))
        val first     = report(old, YearMonth.of(2025, 2)).copy(settled = true)
        val latest    = report(old, YearMonth.of(2025, 3)).copy(settled = true)
        val requested = report(old)
        val other     = old.copy(id = InvestmentScreenId("other"))
        for
          screens        <- InvestmentScreenRepository.make[IO](db)
          repo           <- InvestmentEvaluationRepository.make[IO](db)
          _              <- List(old, other).traverse_(screens.save)
          _              <- List(first, latest, requested, report(other).copy(settled = true)).traverse_(repo.saveMonthly)
          _              <- repo.recordHistoryRequests(old.id, month, List(AAPL, MSFT))
          context        <- repo.findContext(old.id, month)
          withoutMonthly <- repo.findContext(old.id, month.plusMonths(1))
        yield
          first.isFinished mustBe true
          latest.isFinished mustBe true
          context mustBe Some(EvaluationCapture.Context(old, Some(requested), Some(latest)))
          withoutMonthly mustBe Some(EvaluationCapture.Context(old, None, Some(latest)))
      }

    "select each current or unfinished month once and resume old captures even after a finished capture" in
      withEmbeddedMongoDatabase { db =>
        val prior           = month.minusMonths(1)
        val fresh           = current.copy(id = InvestmentScreenId("fresh"))
        val settled         = current.copy(id = InvestmentScreenId("settled"))
        val finished        = current.copy(id = InvestmentScreenId("finished"), createdAt = Instant.parse("2020-01-01T00:00:00Z"))
        val pendingCurrent  = report(current)
        val pendingPrior    = report(current, prior)
        val pendingFinished = report(finished, prior)
        for
          screens <- InvestmentScreenRepository.make[IO](db)
          repo    <- InvestmentEvaluationRepository.make[IO](db)
          _       <- List(current, fresh, settled, finished).traverse_(screens.save)
          _       <- List(
            pendingCurrent,
            pendingPrior,
            report(settled).copy(settled = true),
            report(finished, YearMonth.of(2025, 2)).copy(settled = true),
            pendingFinished
          ).traverse_(repo.saveMonthly)
          _     <- repo.recordHistoryRequests(current.id, prior, List(AAPL))
          tasks <- repo.captureTasks(month)
        yield
          tasks.map(t => t.screen.id -> t.month).toSet mustBe Set(
            current.id  -> month,
            current.id  -> prior,
            fresh.id    -> month,
            finished.id -> prior
          )
          tasks.size mustBe 4
          tasks.find(t => t.screen.id == current.id && t.month == prior).flatMap(_.existing) mustBe
            Some(EvaluationCapture.State(Set(AAPL)))
          tasks.find(t => t.screen.id == current.id && t.month == month).flatMap(_.existing) mustBe
            Some(EvaluationCapture.State(Set.empty))
      }

    "plan captures without embedding historical reports beyond Mongo's document limit" in
      withEmbeddedMongoDatabase { db =>
        val old         = current.copy(createdAt = Instant.parse("2026-01-01T00:00:00Z"))
        val months      = List(YearMonth.of(2026, 2), YearMonth.of(2026, 3), YearMonth.of(2026, 4))
        val methodology = "x" * (6 * 1024 * 1024)
        for
          screens <- InvestmentScreenRepository.make[IO](db)
          repo    <- InvestmentEvaluationRepository.make[IO](db)
          _       <- screens.save(old)
          _       <- months.traverse_(m => repo.saveMonthly(report(old, m).copy(methodology = methodology)))
          context <- repo.findContext(old.id, month)
          tasks   <- repo.captureTasks(month)
        yield
          context mustBe Some(EvaluationCapture.Context(old, None, None))
          tasks.map(_.month).toSet mustBe (months :+ month).toSet
          tasks.forall(_.existing.forall(_.requestedHistory.isEmpty)) mustBe true
      }

    "retain batched history requests across concurrent additions and capture saves" in
      withEmbeddedMongoDatabase { db =>
        val pending = report()
        for
          screens    <- InvestmentScreenRepository.make[IO](db)
          repo       <- InvestmentEvaluationRepository.make[IO](db)
          _          <- screens.save(current)
          _          <- repo.saveMonthly(pending)
          initial    <- repo.captureTasks(month)
          _          <- List(List(AAPL, MSFT), List(AAPL), Nil).parTraverse_(repo.recordHistoryRequests(current.id, month, _))
          _          <- repo.saveMonthly(pending.copy(methodology = "stale draft"))
          requested  <- repo.captureTasks(month)
          _          <- repo.saveMonthly(pending.copy(settled = true))
          _          <- repo.saveMonthly(pending)
          settled    <- repo.findContext(current.id, month)
          captures   <- db.getCollectionWithCodec[InvestmentEvaluationEntity](InvestmentEvaluationRepository.CollectionName)
          stored     <- captures.find(Filter.idEq(s"${current.id.value}:$month")).first
          otherMonth <- repo.findContext(current.id, month.plusMonths(1))
        yield
          initial.flatMap(_.existing).map(_.requestedHistory) mustBe List(Set.empty)
          requested.flatMap(_.existing).map(_.requestedHistory) mustBe List(Set(AAPL, MSFT))
          settled.flatMap(_.monthly) mustBe Some(pending.copy(settled = true))
          stored.map(_.requestedHistory.toSet) mustBe Some(Set(AAPL, MSFT))
          otherMonth.flatMap(_.monthly) mustBe None
      }

    "accept concurrent initial captures and keep a settled winner immutable" in
      withEmbeddedMongoDatabase { db =>
        val pending  = report()
        val complete = pending.copy(settled = true, methodology = "settled outcome")
        for
          screens    <- InvestmentScreenRepository.make[IO](db)
          repo       <- InvestmentEvaluationRepository.make[IO](db)
          _          <- screens.save(current)
          _          <- (1 to 32).toList.parTraverse_(n => repo.saveMonthly(pending.copy(methodology = s"attempt-$n")))
          unfinished <- repo.captureTasks(month)
          _          <- List(complete, pending).parTraverse_(repo.saveMonthly)
          _          <- repo.saveMonthly(pending)
          saved      <- repo.findContext(current.id, month)
          remaining  <- repo.captureTasks(month)
        yield
          unfinished.size mustBe 1
          saved.flatMap(_.monthly) mustBe Some(complete)
          remaining mustBe Nil
      }

    "read adjusted snapshots in one batch only when their ranges cover completed sessions" in
      withEmbeddedMongoDatabase { db =>
        val from         = LocalDate.parse("2026-08-01")
        val through      = LocalDate.parse("2026-09-30")
        val range        = DateRange(from, through)
        val cutoff       = through.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant
        val good         = snapshot(AAPL, range, cutoff)
        val early        = snapshot(Ticker("EARLY"), range, cutoff.minusMillis(1))
        val lateStart    = snapshot(Ticker("FROM"), range.copy(from = from.plusDays(1)))
        val short        = snapshot(Ticker("THROUGH"), range.copy(through = through.minusDays(1)))
        val unadjusted   = snapshot(MSFT, range, adjustment = PriceAdjustment.Unadjusted)
        val other        = snapshot(Ticker("OTHER"), range)
        val observations = List(good, early, lateStart, short, unadjusted, other)
        for
          prices <- PriceHistoryRepository.make[IO](db)
          repo   <- InvestmentEvaluationRepository.make[IO](db)
          _      <- observations.traverse_(prices.save)
          found  <- repo.findPriceSeries(observations.filterNot(_ == other).map(_.ticker), range)
          empty  <- repo.findPriceSeries(Nil, range)
        yield
          found.keySet mustBe Set(AAPL)
          found(AAPL).snapshotIds mustBe List(good.id)
          found(AAPL).fetchedAt mustBe cutoff
          found(AAPL).audited mustBe true
          empty mustBe Map.empty
      }

    "trim candles inclusively while auditing the original provider response size" in
      withEmbeddedMongoDatabase { db =>
        val from        = LocalDate.parse("2026-01-01")
        val through     = from.plusDays(3)
        val range       = DateRange(from, through)
        val fullRange   = DateRange(from.minusYears(15), through.plusMonths(1))
        val candles     = (0 until 5001).toList.map(n => PriceCandle(from.minusDays(n.toLong).plusDays(3), 100, 100, 100, 100, 1L))
        val truncated   = snapshot(AAPL, fullRange).copy(candles = candles)
        val unsupported = snapshot(MSFT, fullRange).copy(currency = "EUR")
        for
          prices <- PriceHistoryRepository.make[IO](db)
          repo   <- InvestmentEvaluationRepository.make[IO](db)
          _      <- List(truncated, unsupported).traverse_(prices.save)
          found  <- repo.findPriceSeries(List(AAPL, MSFT), range)
        yield
          found(AAPL).closes.map(_.date).toSet mustBe (0 to 3).map(n => from.plusDays(n.toLong)).toSet
          found(AAPL).completeRange mustBe fullRange
          found(AAPL).audited mustBe false
          found(AAPL).issues mustBe List("provider response may be truncated")
          found(AAPL).snapshotIds mustBe List(truncated.id)
          found(MSFT).audited mustBe false
          found(MSFT).issues mustBe List("unsupported evaluation currency")
      }
  }
