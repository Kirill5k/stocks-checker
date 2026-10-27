package stockschecker.repositories

import cats.effect.IO
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.operations.{Filter, Update}
import stockschecker.domain.*
import stockschecker.fixtures.*
import stockschecker.utils.InvestmentEvaluator

import java.time.{Instant, LocalDate}
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
          "audited",
          now,
          true,
          "split-and-dividend",
          DateRange(from, through)
        )
        val evaluation = InvestmentEvaluator.evaluate(
          screenRun.id,
          formedAt,
          now,
          screenRun.cohorts,
          Map(AAPL -> series, InvestmentEvaluator.Benchmark -> series.copy(ticker = InvestmentEvaluator.Benchmark)),
          Some(through)
        )
        for
          repo      <- InvestmentEvaluationRepository.make[IO](db)
          _         <- repo.saveMonthly(evaluation)
          overwrite <- repo.saveMonthly(evaluation.copy(methodology = "changed")).attempt
          saved     <- repo.findMonthly(screenRun.id, "2026-10")
          raw       <- db.getCollection(InvestmentEvaluationRepository.CollectionName)
          stored    <- raw
            .find(
              Filter.idEq(s"${screenRun.id.value}:2026-10") && Filter.eq("screenId", screenRun.id.value) &&
                Filter.eq("evaluation.screenId", screenRun.id.value)
            )
            .first
        yield
          overwrite.isRight mustBe true
          saved mustBe Some(evaluation)
          stored must not be empty
      }
    "mark completed legacy captures so subsequent retries exclude them" in
      withEmbeddedMongoDatabase { db =>
        val formedAt = Instant.parse("2026-09-01T00:00:00Z")
        val from     = LocalDate.parse("2026-09-01")
        val through  = LocalDate.parse("2026-10-01")
        val series   = EvaluationPriceSeries(
          AAPL,
          List(EvaluationClose(from.plusDays(1), 100), EvaluationClose(through, 110)),
          "audited",
          now,
          true,
          "split-and-dividend",
          DateRange(from, through)
        )
        val evaluation = InvestmentEvaluator.evaluate(
          screenRun.id,
          formedAt,
          now,
          screenRun.cohorts,
          Map(AAPL -> series, InvestmentEvaluator.Benchmark -> series.copy(ticker = InvestmentEvaluator.Benchmark)),
          Some(through)
        )
        for
          repo    <- InvestmentEvaluationRepository.make[IO](db)
          _       <- repo.saveMonthly(evaluation)
          entries <- db.getCollectionWithCodec[entities.InvestmentEvaluationEntity](InvestmentEvaluationRepository.CollectionName)
          _ <- entries.updateOne(Filter.idEq(s"${screenRun.id.value}:2026-10"), Update.unset("complete").unset("settled").unset("finished"))
          pending   <- repo.unsettled
          migrated  <- entries.find(Filter.idEq(s"${screenRun.id.value}:2026-10")).first
          remaining <- entries.find(Filter.ne("settled", true)).all
        yield
          pending mustBe Nil
          migrated.flatMap(_.settled) mustBe Some(true)
          migrated.map(_.evaluation) mustBe Some(evaluation)
          remaining mustBe empty
      }
    "freeze settled missing outcomes and mark finished screens only after the full exit window" in
      withEmbeddedMongoDatabase { db =>
        val formedAt = Instant.parse("2021-01-01T00:00:00Z")
        val through  = LocalDate.parse("2026-01-04")
        val from     = LocalDate.parse("2021-01-01")
        val series   = EvaluationPriceSeries(
          InvestmentEvaluator.Benchmark,
          List("2021-01-04", "2024-01-04", "2026-01-02").map(day => EvaluationClose(LocalDate.parse(day), 100)),
          "audited",
          now,
          true,
          "split-and-dividend",
          DateRange(from, through)
        )
        val missingExit = InvestmentEvaluator
          .evaluate(screenRun.id, formedAt, now, screenRun.cohorts, Map(InvestmentEvaluator.Benchmark -> series), Some(through))
          .copy(captureMonth = Some("2026-01"), settled = Some(true))
        val finalMissing = missingExit.copy(captureMonth = Some("2026-02"), dataThrough = Some(through.plusDays(7)))
        for
          repo       <- InvestmentEvaluationRepository.make[IO](db)
          _          <- repo.saveMonthly(missingExit)
          _          <- repo.saveMonthly(missingExit.copy(methodology = "must remain frozen"))
          saved      <- repo.findMonthly(screenRun.id, "2026-01")
          unfinished <- repo.finishedScreenIds
          pending    <- repo.unsettled
          _          <- repo.saveMonthly(finalMissing)
          finished   <- repo.finishedScreenIds
          latest     <- repo.findFinished(screenRun.id)
        yield
          missingExit.horizons.last.targetDate mustBe through
          missingExit.horizons.last.exitDate mustBe None
          saved mustBe Some(missingExit)
          unfinished mustBe empty
          pending mustBe Nil
          finished mustBe Set(screenRun.id)
          latest mustBe Some(finalMissing)
      }
    "finish missing-entry captures at the same conservative cutoff across leap years" in
      withEmbeddedMongoDatabase { db =>
        val formedAt   = Instant.parse("2019-02-28T00:00:00Z")
        val formedDate = LocalDate.parse("2019-02-28")
        val through    = InvestmentEvaluator.finalCaptureThrough(formedDate)
        val evaluation = InvestmentEvaluator
          .evaluate(screenRun.id, formedAt, now, screenRun.cohorts, Map.empty, Some(through))
          .copy(captureMonth = Some("2024-04"), settled = Some(true))
        for
          repo     <- InvestmentEvaluationRepository.make[IO](db)
          _        <- repo.saveMonthly(evaluation)
          finished <- repo.finishedScreenIds
        yield
          through mustBe LocalDate.parse("2024-03-15")
          through.isBefore(formedDate.plusDays(7).plusYears(5).plusDays(7)) mustBe false
          finished mustBe Set(screenRun.id)
      }
    "replace incomplete legacy captures while retaining the successful monthly result" in
      withEmbeddedMongoDatabase { db =>
        val formedAt   = Instant.parse("2026-09-01T00:00:00Z")
        val from       = LocalDate.parse("2026-09-01")
        val through    = LocalDate.parse("2026-10-01")
        val incomplete = InvestmentEvaluator.evaluate(screenRun.id, formedAt, now, screenRun.cohorts, Map.empty, Some(through))
        val series     = EvaluationPriceSeries(
          AAPL,
          List(EvaluationClose(from.plusDays(1), 100), EvaluationClose(through, 110)),
          "audited",
          now,
          true,
          "split-and-dividend",
          DateRange(from, through)
        )
        val complete = InvestmentEvaluator.evaluate(
          screenRun.id,
          formedAt,
          now,
          screenRun.cohorts,
          Map(AAPL -> series, InvestmentEvaluator.Benchmark -> series.copy(ticker = InvestmentEvaluator.Benchmark)),
          Some(through)
        )
        for
          repo      <- InvestmentEvaluationRepository.make[IO](db)
          _         <- repo.saveMonthly(incomplete)
          pending   <- repo.unsettled
          _         <- repo.saveMonthly(complete)
          saved     <- repo.findMonthly(screenRun.id, "2026-10")
          remaining <- repo.unsettled
          overwrite <- repo.saveMonthly(complete.copy(methodology = "changed")).attempt
        yield
          pending mustBe List(incomplete)
          saved mustBe Some(complete)
          remaining mustBe Nil
          overwrite.isRight mustBe true
      }
  }
