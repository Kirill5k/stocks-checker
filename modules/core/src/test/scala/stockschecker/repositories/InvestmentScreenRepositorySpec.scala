package stockschecker.repositories

import cats.effect.IO
import cats.syntax.traverse.*
import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.operations.{Filter, Update}
import stockschecker.domain.*
import stockschecker.fixtures.*
import java.time.{Instant, LocalDate}

class InvestmentScreenRepositorySpec extends RepositorySpec with MongoJsonCodecs:
  val port         = 12164
  private val now  = Instant.parse("2026-10-19T12:00:00Z")
  private val card = InvestmentScorecard(
    AAPL,
    true,
    Some(BigDecimal(70)),
    Map("quality" -> BigDecimal(80)),
    Map.empty,
    Nil,
    Nil,
    Some("industry:test"),
    20,
    None,
    "fundamental-v1"
  )
  private val run = InvestmentScreenRun(
    InvestmentScreenId("immutable-run"),
    now,
    InvestmentRecipe(),
    InvestmentScreenRequest(),
    List(InvestmentCandidate(AAPLStock, None)),
    List(card),
    List(InvestmentCohort("investment", List(AAPL))),
    "rank-1"
  )

  "An InvestmentScreenRepository" should {
    "round trip the frozen input universe and reject overwriting a published screen" in
      withEmbeddedMongoDatabase { db =>
        for
          repo      <- InvestmentScreenRepository.make[IO](db)
          ranks     <- InvestmentRankingRepository.make[IO](db)
          _         <- ranks.save(InvestmentRankingRun(run.rankingId, now, run.recipe, run.inputs, run.scorecards))
          _         <- repo.save(run)
          overwrite <- repo.save(run.copy(scorecards = List(card.copy(score = Some(BigDecimal(99)))))).attempt
          saved     <- repo.find(run.id)
          listed    <- repo.list(20)
          raw       <- db.getCollection(InvestmentScreenRepository.CollectionName)
          stored    <- raw.find(Filter.idEq(run.id.value) && Filter.eq("summary.id", run.id.value)).first
        yield
          overwrite.isLeft mustBe true
          saved mustBe Some(run)
          listed.map(_.id) mustBe List(run.id)
          stored must not be empty
      }
    "read existing UUID and monthly screen IDs stored as BSON strings" in
      withEmbeddedMongoDatabase { db =>
        val ids = List("e7ce580d-1cdf-4219-9d34-1a25437f05b1", "monthly:2026-10:quality-fair-value-v1").map(InvestmentScreenId(_))
        for
          repo  <- InvestmentScreenRepository.make[IO](db)
          ranks <- InvestmentRankingRepository.make[IO](db)
          _     <- ranks.save(InvestmentRankingRun(run.rankingId, now, run.recipe, run.inputs, run.scorecards))
          raw   <- db.getCollection(InvestmentScreenRepository.CollectionName)
          _     <- raw.insertMany(ids.map { id =>
            Document(
              "_id"       := id.value,
              "createdAt" := now,
              "rankingId" := run.rankingId,
              "request"   := Document(),
              "cohorts"   := List(Document("name" := "investment", "tickers" := List(AAPL.value))),
              "summary"   := Document(
                "id"               := id.value,
                "createdAt"        := now.toString,
                "modelVersion"     := run.recipe.version,
                "universeCount"    := 1,
                "eligibleCount"    := 1,
                "selectedCount"    := 1,
                "excludedByReason" := Document()
              )
            )
          })
          saved  <- ids.traverse(repo.find)
          allIds <- repo.ids
          listed <- repo.list(20)
        yield
          saved mustBe ids.map(id => Some(run.copy(id = id)))
          allIds.toSet mustBe ids.toSet
          listed.map(_.id).toSet mustBe ids.toSet
      }
    "not expose an input batch without its completed screen header" in
      withEmbeddedMongoDatabase { db =>
        for
          repo   <- InvestmentScreenRepository.make[IO](db)
          saved  <- repo.find(InvestmentScreenId("unfinished"))
          listed <- repo.list(20)
        yield
          saved mustBe None
          listed mustBe Nil
      }
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
          run.id,
          formedAt,
          now,
          run.cohorts,
          Map(AAPL -> series, InvestmentEvaluator.Benchmark -> series.copy(ticker = InvestmentEvaluator.Benchmark)),
          Some(through)
        )
        for
          repo      <- InvestmentEvaluationRepository.make[IO](db)
          _         <- repo.getOrCreateCapturePlan(EvaluationCapturePlan(run.id, "2026-10", through, now))
          _         <- repo.saveMonthly(evaluation)
          overwrite <- repo.saveMonthly(evaluation.copy(methodology = "changed")).attempt
          saved     <- repo.findMonthly(run.id, "2026-10")
          plan      <- repo.findCapturePlan(run.id, "2026-10")
          raw       <- db.getCollection(InvestmentEvaluationRepository.CollectionName)
          stored    <- raw
            .find(
              Filter.idEq(s"${run.id.value}:2026-10") && Filter.eq("screenId", run.id.value) &&
                Filter.eq("evaluation.screenId", run.id.value)
            )
            .first
        yield
          overwrite.isRight mustBe true
          saved mustBe Some(evaluation)
          plan mustBe None
          stored must not be empty
      }
    "mark completed legacy captures so subsequent retries exclude them and retire their plans" in
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
          run.id,
          formedAt,
          now,
          run.cohorts,
          Map(AAPL -> series, InvestmentEvaluator.Benchmark -> series.copy(ticker = InvestmentEvaluator.Benchmark)),
          Some(through)
        )
        for
          repo    <- InvestmentEvaluationRepository.make[IO](db)
          _       <- repo.saveMonthly(evaluation)
          entries <- db.getCollectionWithCodec[InvestmentEvaluationRepository.Entry](InvestmentEvaluationRepository.CollectionName)
          _       <- entries.updateOne(Filter.idEq(s"${run.id.value}:2026-10"), Update.unset("complete").unset("settled").unset("finished"))
          _       <- repo.getOrCreateCapturePlan(EvaluationCapturePlan(run.id, "2026-10", through, now))
          pending <- repo.unsettled
          migrated  <- entries.find(Filter.idEq(s"${run.id.value}:2026-10")).first
          remaining <- entries.find(Filter.ne("settled", true)).all
          plan      <- repo.findCapturePlan(run.id, "2026-10")
        yield
          pending mustBe Nil
          migrated.flatMap(_.settled) mustBe Some(true)
          migrated.map(_.evaluation) mustBe Some(evaluation)
          remaining mustBe empty
          plan mustBe None
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
          .evaluate(run.id, formedAt, now, run.cohorts, Map(InvestmentEvaluator.Benchmark -> series), Some(through))
          .copy(captureMonth = Some("2026-01"), settled = Some(true))
        val finalMissing = missingExit.copy(captureMonth = Some("2026-02"), dataThrough = Some(through.plusDays(7)))
        for
          repo       <- InvestmentEvaluationRepository.make[IO](db)
          _          <- repo.saveMonthly(missingExit)
          _          <- repo.saveMonthly(missingExit.copy(methodology = "must remain frozen"))
          saved      <- repo.findMonthly(run.id, "2026-01")
          unfinished <- repo.finishedScreenIds
          pending    <- repo.unsettled
          _          <- repo.saveMonthly(finalMissing)
          finished   <- repo.finishedScreenIds
          latest     <- repo.findFinished(run.id)
        yield
          missingExit.horizons.last.targetDate mustBe through
          missingExit.horizons.last.exitDate mustBe None
          saved mustBe Some(missingExit)
          unfinished mustBe empty
          pending mustBe Nil
          finished mustBe Set(run.id)
          latest mustBe Some(finalMissing)
      }
    "finish missing-entry captures at the same conservative cutoff across leap years" in
      withEmbeddedMongoDatabase { db =>
        val formedAt   = Instant.parse("2019-02-28T00:00:00Z")
        val formedDate = LocalDate.parse("2019-02-28")
        val through    = InvestmentEvaluator.finalCaptureThrough(formedDate)
        val evaluation = InvestmentEvaluator
          .evaluate(run.id, formedAt, now, run.cohorts, Map.empty, Some(through))
          .copy(captureMonth = Some("2024-04"), settled = Some(true))
        for
          repo     <- InvestmentEvaluationRepository.make[IO](db)
          _        <- repo.saveMonthly(evaluation)
          finished <- repo.finishedScreenIds
        yield
          through mustBe LocalDate.parse("2024-03-15")
          through.isBefore(formedDate.plusDays(7).plusYears(5).plusDays(7)) mustBe false
          finished mustBe Set(run.id)
      }
    "replace incomplete legacy captures while retaining the successful monthly result" in
      withEmbeddedMongoDatabase { db =>
        val formedAt   = Instant.parse("2026-09-01T00:00:00Z")
        val from       = LocalDate.parse("2026-09-01")
        val through    = LocalDate.parse("2026-10-01")
        val incomplete = InvestmentEvaluator.evaluate(run.id, formedAt, now, run.cohorts, Map.empty, Some(through))
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
          run.id,
          formedAt,
          now,
          run.cohorts,
          Map(AAPL -> series, InvestmentEvaluator.Benchmark -> series.copy(ticker = InvestmentEvaluator.Benchmark)),
          Some(through)
        )
        for
          repo      <- InvestmentEvaluationRepository.make[IO](db)
          _         <- repo.saveMonthly(incomplete)
          pending   <- repo.unsettled
          _         <- repo.saveMonthly(complete)
          saved     <- repo.findMonthly(run.id, "2026-10")
          remaining <- repo.unsettled
          overwrite <- repo.saveMonthly(complete.copy(methodology = "changed")).attempt
        yield
          pending mustBe List(incomplete)
          saved mustBe Some(complete)
          remaining mustBe Nil
          overwrite.isRight mustBe true
      }
    "store only financial diagnostics alongside the scorecard evidence and expose lightweight ranking headers" in
      withEmbeddedMongoDatabase { db =>
        val observation = FinancialObservation("fcfMargin", LocalDate.parse("2025-12-31"), BigDecimal("0.2"), "ratio")
        val snapshot    = FinancialSnapshot(
          "snapshot",
          AAPL,
          "Finnhub",
          now,
          Map("peTTM" -> BigDecimal(10), "irrelevant" -> BigDecimal(123)),
          List(observation),
          List(observation),
          "large raw provider response",
          "hash",
          "mapping",
          Nil
        )
        val candidate = InvestmentCandidate(AAPLStock, Some(snapshot))
        val evidence  = InvestmentEvidence(
          "Finnhub",
          now,
          "mapping",
          "hash",
          Map("fcfMargin" -> List(observation.period)),
          List(observation),
          None,
          None,
          Some("USD")
        )
        val ranking = InvestmentRankingRun(
          "compact",
          now,
          InvestmentRecipe(),
          List(candidate),
          List(card.copy(financialSnapshotId = Some(snapshot.id), evidence = Some(evidence)))
        )
        for
          repo   <- InvestmentRankingRepository.make[IO](db)
          _      <- repo.save(ranking)
          header <- repo.latestHeader
          saved  <- repo.find(ranking.id)
        yield
          header.map(_.summary) mustBe Some(InvestmentRankingSummary(1, 1, Map.empty))
          saved.get.inputs.head.financialSnapshot.get.annual mustBe Nil
          saved.get.inputs.head.financialSnapshot.get.quarterly mustBe Nil
          saved.get.inputs.head.financialSnapshot.get.rawPayload mustBe ""
          saved.get.inputs.head.financialSnapshot.get.metrics mustBe Map("peTTM" -> BigDecimal(10))
          saved.get.scorecards mustBe ranking.scorecards
      }
    "backfill legacy header summaries without losing published rankings" in
      withEmbeddedMongoDatabase { db =>
        val ranking = InvestmentRankingRun("legacy", now, InvestmentRecipe(), run.inputs, run.scorecards)
        for
          repo     <- InvestmentRankingRepository.make[IO](db)
          _        <- repo.save(ranking)
          raw      <- db.getCollection(InvestmentRankingRepository.CollectionName)
          _        <- raw.updateOne(Filter.idEq(ranking.id), Update.unset("summary"))
          header   <- repo.latestHeader
          restored <- repo.find(ranking.id)
        yield
          header.map(_.summary) mustBe Some(InvestmentRankingSummary(1, 1, Map.empty))
          restored mustBe Some(ranking)
      }
    "prune old unreferenced rankings and interrupted inputs while preserving screen evidence" in
      withEmbeddedMongoDatabase { db =>
        val old     = now.minusSeconds(40L * 86400)
        val ranking = InvestmentRankingRun(run.rankingId, old, run.recipe, run.inputs, run.scorecards)
        for
          repo     <- InvestmentRankingRepository.make[IO](db)
          screens  <- InvestmentScreenRepository.make[IO](db)
          _        <- repo.save(ranking)
          _        <- screens.save(run)
          _        <- repo.save(ranking.copy(id = "obsolete"))
          _        <- repo.save(ranking.copy(id = "recent", createdAt = now))
          _        <- repo.save(ranking.copy(id = "interrupted"))
          headers  <- db.getCollection(InvestmentRankingRepository.CollectionName)
          _        <- headers.deleteMany(Filter.idEq("interrupted"))
          _        <- repo.prune(now.minusSeconds(30L * 86400))
          retained <- repo.find(run.rankingId)
          obsolete <- repo.find("obsolete")
          recent   <- repo.find("recent")
          inputs   <- db.getCollectionWithCodec[InvestmentRankingRepository.Input](InvestmentRankingRepository.InputCollectionName)
          rows     <- inputs.find.all
        yield
          retained mustBe Some(ranking)
          obsolete mustBe None
          recent.map(_.id) mustBe Some("recent")
          rows.map(_.rankingId).toSet mustBe Set(run.rankingId, "recent")
      }
    "retain the original monthly price cutoff when deferred work resumes on a later date" in
      withEmbeddedMongoDatabase { db =>
        val original = EvaluationCapturePlan(run.id, "2026-10", LocalDate.parse("2026-10-20"), now)
        for
          repo   <- InvestmentEvaluationRepository.make[IO](db)
          first  <- repo.getOrCreateCapturePlan(original)
          later  <- repo.getOrCreateCapturePlan(original.copy(through = LocalDate.parse("2026-10-25")))
          stored <- repo.findCapturePlan(run.id, "2026-10")
          raw    <- db.getCollection(InvestmentEvaluationRepository.PlanCollectionName)
          plan   <- raw.find(Filter.idEq(s"${run.id.value}:2026-10") && Filter.eq("plan.screenId", run.id.value)).first
        yield
          first mustBe original
          later mustBe original
          stored mustBe Some(original)
          plan must not be empty
      }
  }
