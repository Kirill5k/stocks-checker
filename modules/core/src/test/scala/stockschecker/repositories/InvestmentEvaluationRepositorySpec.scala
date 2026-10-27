package stockschecker.repositories

import cats.effect.IO
import cats.syntax.parallel.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.operations.Filter
import stockschecker.domain.*
import stockschecker.fixtures.*
import stockschecker.domain.investment.InvestmentEvaluator

import java.time.{Instant, LocalDate, YearMonth}
import stockschecker.repositories.InvestmentRepositoryFixtures.*

class InvestmentEvaluationRepositorySpec extends RepositorySpec with MongoJsonCodecs:
  override val port = 12221

  "An InvestmentEvaluationRepository" should {
    "save a single immutable evaluation for a screen and month" in
      withEmbeddedMongoDatabase { db =>
        val formedAt = Instant.parse("2026-09-01T00:00:00Z")
        val from     = LocalDate.parse("2026-09-01")
        val through  = LocalDate.parse("2026-10-01")
        val series   = EvaluationPriceSeries(
          AAPL,
          List(EvaluationClose(from.plusDays(1), 100), EvaluationClose(through, 110)),
          Provider.TwelveData,
          now,
          true,
          PriceAdjustment.SplitsAndDividends,
          DateRange(from, through)
        )
        val evaluation = InvestmentEvaluator
          .evaluate(
            screen.id,
            formedAt,
            now,
            screen.cohorts,
            Map(AAPL -> series, InvestmentEvaluator.Benchmark -> series.copy(ticker = InvestmentEvaluator.Benchmark)),
            Some(through)
          )
          .copy(settled = true)
        for
          repo      <- InvestmentEvaluationRepository.make[IO](db)
          _         <- repo.saveMonthly(evaluation)
          overwrite <- repo.saveMonthly(evaluation.copy(methodology = "changed")).attempt
          saved     <- repo.findMonthly(screen.id, YearMonth.parse("2026-10"))
          raw       <- db.getCollection(InvestmentEvaluationRepository.CollectionName)
          stored    <- raw
            .find(
              Filter.idEq(s"${screen.id.value}:2026-10") && Filter.eq("screenId", screen.id.value) &&
                Filter.eq("evaluation.screenId", screen.id.value)
            )
            .first
        yield
          overwrite.isRight mustBe true
          saved mustBe Some(evaluation)
          stored must not be empty
      }
    "freeze settled missing outcomes and mark finished screens only after the full exit window" in
      withEmbeddedMongoDatabase { db =>
        val formedAt = Instant.parse("2021-01-01T00:00:00Z")
        val through  = LocalDate.parse("2026-01-04")
        val from     = LocalDate.parse("2021-01-01")
        val series   = EvaluationPriceSeries(
          InvestmentEvaluator.Benchmark,
          List("2021-01-04", "2024-01-04", "2026-01-02").map(day => EvaluationClose(LocalDate.parse(day), 100)),
          Provider.TwelveData,
          now,
          true,
          PriceAdjustment.SplitsAndDividends,
          DateRange(from, through)
        )
        val missingExit = InvestmentEvaluator
          .evaluate(screen.id, formedAt, now, screen.cohorts, Map(InvestmentEvaluator.Benchmark -> series), Some(through))
          .copy(captureMonth = Some(YearMonth.parse("2026-01")), settled = true)
        val finalMissing = missingExit.copy(captureMonth = Some(YearMonth.parse("2026-02")), dataThrough = Some(through.plusDays(7)))
        for
          repo       <- InvestmentEvaluationRepository.make[IO](db)
          _          <- repo.saveMonthly(missingExit)
          _          <- repo.saveMonthly(missingExit.copy(methodology = "must remain frozen"))
          saved      <- repo.findMonthly(screen.id, YearMonth.parse("2026-01"))
          unfinished <- repo.finishedScreenIds
          pending    <- repo.unsettled
          _          <- repo.saveMonthly(finalMissing)
          finished   <- repo.finishedScreenIds
          latest     <- repo.findFinished(screen.id)
        yield
          missingExit.horizons.last.targetDate mustBe through
          missingExit.horizons.last.exitDate mustBe None
          saved mustBe Some(missingExit)
          unfinished mustBe empty
          pending mustBe Nil
          finished mustBe Set(screen.id)
          latest mustBe Some(finalMissing)
      }
    "finish missing-entry captures at the same conservative cutoff across leap years" in
      withEmbeddedMongoDatabase { db =>
        val formedAt   = Instant.parse("2019-02-28T00:00:00Z")
        val formedDate = LocalDate.parse("2019-02-28")
        val through    = InvestmentEvaluation.finalCaptureThrough(formedDate)
        val evaluation = InvestmentEvaluator
          .evaluate(screen.id, formedAt, now, screen.cohorts, Map.empty, Some(through))
          .copy(captureMonth = Some(YearMonth.parse("2024-04")), settled = true)
        for
          repo     <- InvestmentEvaluationRepository.make[IO](db)
          _        <- repo.saveMonthly(evaluation)
          finished <- repo.finishedScreenIds
        yield
          through mustBe LocalDate.parse("2024-03-15")
          through.isBefore(formedDate.plusDays(7).plusYears(5).plusDays(7)) mustBe false
          finished mustBe Set(screen.id)
      }
    "replace an unsettled capture and freeze its settled result" in
      withEmbeddedMongoDatabase { db =>
        val formedAt   = Instant.parse("2026-09-01T00:00:00Z")
        val from       = LocalDate.parse("2026-09-01")
        val through    = LocalDate.parse("2026-10-01")
        val incomplete = InvestmentEvaluator.evaluate(screen.id, formedAt, now, screen.cohorts, Map.empty, Some(through))
        val series     = EvaluationPriceSeries(
          AAPL,
          List(EvaluationClose(from.plusDays(1), 100), EvaluationClose(through, 110)),
          Provider.TwelveData,
          now,
          true,
          PriceAdjustment.SplitsAndDividends,
          DateRange(from, through)
        )
        val complete = InvestmentEvaluator
          .evaluate(
            screen.id,
            formedAt,
            now,
            screen.cohorts,
            Map(AAPL -> series, InvestmentEvaluator.Benchmark -> series.copy(ticker = InvestmentEvaluator.Benchmark)),
            Some(through)
          )
          .copy(settled = true)
        for
          repo      <- InvestmentEvaluationRepository.make[IO](db)
          _         <- repo.saveMonthly(incomplete)
          pending   <- repo.unsettled
          _         <- repo.saveMonthly(complete)
          saved     <- repo.findMonthly(screen.id, YearMonth.parse("2026-10"))
          remaining <- repo.unsettled
          overwrite <- repo.saveMonthly(complete.copy(methodology = "changed")).attempt
        yield
          pending mustBe List(incomplete)
          saved mustBe Some(complete)
          remaining mustBe Nil
          overwrite.isRight mustBe true
      }
    "accept concurrent initial captures and keep a settled winner immutable" in
      withEmbeddedMongoDatabase { db =>
        val pending = InvestmentEvaluator
          .evaluate(screen.id, screen.createdAt, now, screen.cohorts, Map.empty, Some(LocalDate.parse("2026-09-30")))
          .copy(captureMonth = Some(YearMonth.of(2026, 10)))
        val complete = pending.copy(settled = true, methodology = "settled outcome")
        for
          repo       <- InvestmentEvaluationRepository.make[IO](db)
          _          <- (1 to 32).toList.parTraverse_(n => repo.saveMonthly(pending.copy(methodology = s"attempt-$n")))
          unfinished <- repo.unsettled
          _          <- List(complete, pending).parTraverse_(repo.saveMonthly)
          _          <- repo.saveMonthly(pending)
          saved      <- repo.findMonthly(screen.id, YearMonth.of(2026, 10))
        yield
          unfinished.size mustBe 1
          saved mustBe Some(complete)
      }

  }
