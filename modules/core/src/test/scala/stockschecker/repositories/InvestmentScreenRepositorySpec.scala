package stockschecker.repositories

import cats.effect.IO
import cats.syntax.traverse.*
import kirill5k.common.cats.Clock
import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Filter, Update}
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import stockschecker.services.InvestmentRankingService
import stockschecker.utils.InvestmentEvaluator

import java.time.{Instant, LocalDate}

class InvestmentScreenRepositorySpec extends RepositorySpec with MongoJsonCodecs:
  val port         = 12164
  private val now  = Instant.parse("2026-10-19T12:00:00Z")
  given Clock[IO]  = Clock.mock(now)
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
    InvestmentRankingId("rank-1")
  )

  private def rankingService(db: MongoDatabase[IO]): IO[InvestmentRankingService[IO]] =
    for
      stocks   <- StockRepository.make[IO](db)
      history  <- FinancialHistoryRepository.make[IO](db)
      rankings <- InvestmentRankingRepository.make[IO](db)
      inputs   <- InvestmentRankingInputRepository.make[IO](db)
      screens  <- InvestmentScreenRepository.make[IO](db)
      service  <- InvestmentRankingService.make(stocks, history, rankings, inputs, screens)
    yield service

  "An InvestmentScreenRepository" should {
    "round trip screen metadata and reject overwriting a published screen" in
      withEmbeddedMongoDatabase { db =>
        val expected = InvestmentScreenHeader.from(run)
        for
          repo      <- InvestmentScreenRepository.make[IO](db)
          _         <- repo.save(expected)
          overwrite <- repo.save(expected.copy(cohorts = Nil)).attempt
          saved     <- repo.find(run.id)
          header    <- repo.header(run.id)
          listed    <- repo.list(20)
          rankings  <- repo.rankingIds
          raw       <- db.getCollection(InvestmentScreenRepository.CollectionName)
          stored    <- raw
            .find(Filter.idEq(run.id.value) && Filter.eq("summary.id", run.id.value) && Filter.eq("rankingId", run.rankingId.value))
            .first
        yield
          overwrite.isLeft mustBe true
          saved mustBe expected
          header.map(_.id) mustBe Some(run.id)
          header.map(_.rankingId) mustBe Some(run.rankingId)
          listed.map(_.id) mustBe List(run.id)
          rankings mustBe Set(run.rankingId)
          stored must not be empty
      }
    "read existing UUID and monthly screen IDs stored as BSON strings" in
      withEmbeddedMongoDatabase { db =>
        val ids = List("e7ce580d-1cdf-4219-9d34-1a25437f05b1", "monthly:2026-10:quality-fair-value-v1").map(InvestmentScreenId(_))
        for
          repo <- InvestmentScreenRepository.make[IO](db)
          raw  <- db.getCollection(InvestmentScreenRepository.CollectionName)
          _    <- raw.insertMany(ids.map { id =>
            Document(
              "_id"       := id.value,
              "createdAt" := now,
              "rankingId" := run.rankingId.value,
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
          saved mustBe ids.map(id => InvestmentScreenHeader.from(run.copy(id = id)))
          allIds.toSet mustBe ids.toSet
          listed.map(_.id).toSet mustBe ids.toSet
      }
    "return an error when the screen header does not exist" in
      withEmbeddedMongoDatabase { db =>
        val id = InvestmentScreenId("unfinished")
        for
          repo   <- InvestmentScreenRepository.make[IO](db)
          saved  <- repo.find(id).attempt
          listed <- repo.list(20)
        yield
          saved mustBe Left(AppError.EntityDoesNotExist("Investment screen", id.value))
          listed mustBe Nil
      }
    "read screen metadata independently of ranking storage" in
      withEmbeddedMongoDatabase { db =>
        val expected = InvestmentScreenHeader.from(run)
        for
          repo  <- InvestmentScreenRepository.make[IO](db)
          _     <- repo.save(expected)
          saved <- repo.find(run.id)
        yield saved mustBe expected
      }
  }

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
          run.id,
          formedAt,
          now,
          run.cohorts,
          Map(AAPL -> series, InvestmentEvaluator.Benchmark -> series.copy(ticker = InvestmentEvaluator.Benchmark)),
          Some(through)
        )
        for
          repo      <- InvestmentEvaluationRepository.make[IO](db)
          _         <- repo.saveMonthly(evaluation)
          overwrite <- repo.saveMonthly(evaluation.copy(methodology = "changed")).attempt
          saved     <- repo.findMonthly(run.id, "2026-10")
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
          entries <- db.getCollectionWithCodec[entities.InvestmentEvaluationEntity](InvestmentEvaluationRepository.CollectionName)
          _       <- entries.updateOne(Filter.idEq(s"${run.id.value}:2026-10"), Update.unset("complete").unset("settled").unset("finished"))
          pending <- repo.unsettled
          migrated  <- entries.find(Filter.idEq(s"${run.id.value}:2026-10")).first
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
  }

  "An InvestmentRankingInputRepository" should {
    "store only financial diagnostics alongside the scorecard evidence" in
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
          InvestmentRankingId("compact"),
          now,
          InvestmentRecipe(),
          List(candidate),
          List(card.copy(financialSnapshotId = Some(snapshot.id), evidence = Some(evidence)))
        )
        for
          repo    <- InvestmentRankingInputRepository.make[IO](db)
          _       <- repo.save(ranking)
          saved   <- repo.find(ranking.id)
          summary <- repo.summary(ranking.id)
          raw     <- db.getCollection(InvestmentRankingInputRepository.CollectionName)
          stored  <- raw.find(Filter.eq("rankingId", ranking.id.value)).first
        yield
          summary mustBe InvestmentRankingSummary(1, 1, Map.empty)
          stored must not be empty
          saved.inputs.head.financialSnapshot.get.annual mustBe Nil
          saved.inputs.head.financialSnapshot.get.quarterly mustBe Nil
          saved.inputs.head.financialSnapshot.get.rawPayload mustBe ""
          saved.inputs.head.financialSnapshot.get.metrics mustBe Map("peTTM" -> BigDecimal(10))
          saved.scorecards mustBe ranking.scorecards
      }
    "preserve input order independently from scorecard order" in
      withEmbeddedMongoDatabase { db =>
        val ranking = InvestmentRankingRun(
          InvestmentRankingId("different-orders"),
          now,
          run.recipe,
          List(InvestmentCandidate(MSFTStock, None), InvestmentCandidate(AAPLStock, None)),
          List(card, card.copy(ticker = MSFT))
        )
        for
          repo  <- InvestmentRankingInputRepository.make[IO](db)
          _     <- repo.save(ranking)
          saved <- repo.find(ranking.id)
        yield
          saved.inputs mustBe ranking.inputs
          saved.scorecards mustBe ranking.scorecards
      }
  }

  "An InvestmentRankingRepository" should {
    "preserve legacy missing summaries without querying ranking inputs" in
      withEmbeddedMongoDatabase { db =>
        val ranking = InvestmentRankingRun(InvestmentRankingId("legacy-metadata"), now, run.recipe, run.inputs, run.scorecards)
        val header  = InvestmentRankingHeader.from(ranking)
        for
          repo     <- InvestmentRankingRepository.make[IO](db)
          _        <- repo.save(header)
          raw      <- db.getCollection(InvestmentRankingRepository.CollectionName)
          _        <- raw.updateOne(Filter.idEq(ranking.id.value), Update.unset("summary"))
          legacy   <- repo.find(ranking.id)
          _        <- repo.updateSummary(ranking.id, header.summary)
          restored <- repo.latest
        yield
          legacy.map(_.id) mustBe Some(ranking.id)
          legacy.flatMap(_.summary) mustBe None
          restored.flatMap(_.summary) mustBe Some(header.summary)
      }
  }

  "An InvestmentRankingService with MongoDB repositories" should {
    "make a ranking visible only after its header is published" in
      withEmbeddedMongoDatabase { db =>
        val ranking = InvestmentRankingRun(InvestmentRankingId("pending-publication"), now, run.recipe, run.inputs, run.scorecards)
        for
          service     <- rankingService(db)
          inputs      <- InvestmentRankingInputRepository.make[IO](db)
          rankings    <- InvestmentRankingRepository.make[IO](db)
          _           <- inputs.save(ranking)
          before      <- service.latestHeader
          unpublished <- service.get(ranking.id).attempt
          _           <- rankings.save(InvestmentRankingHeader.from(ranking))
          after       <- service.latestHeader
          published   <- service.get(ranking.id)
        yield
          before mustBe None
          unpublished.isLeft mustBe true
          after mustBe Some(InvestmentRankingHeader.from(ranking))
          published mustBe ranking
      }
    "backfill legacy header summaries without losing published rankings" in
      withEmbeddedMongoDatabase { db =>
        val ranking = InvestmentRankingRun(InvestmentRankingId("legacy"), now, InvestmentRecipe(), run.inputs, run.scorecards)
        for
          repo     <- InvestmentRankingRepository.make[IO](db)
          inputs   <- InvestmentRankingInputRepository.make[IO](db)
          service  <- rankingService(db)
          _        <- inputs.save(ranking)
          _        <- repo.save(InvestmentRankingHeader.from(ranking))
          raw      <- db.getCollection(InvestmentRankingRepository.CollectionName)
          _        <- raw.updateOne(Filter.idEq(ranking.id.value), Update.unset("summary"))
          header   <- service.latestHeader
          byId     <- repo.find(ranking.id)
          restored <- service.get(ranking.id)
        yield
          header.map(_.id) mustBe Some(ranking.id)
          header.map(_.summary) mustBe Some(InvestmentRankingSummary(1, 1, Map.empty))
          byId.flatMap(_.summary) mustBe header.map(_.summary)
          restored mustBe ranking
      }
    "prune old unreferenced rankings and interrupted inputs while preserving screen evidence" in
      withEmbeddedMongoDatabase { db =>
        val old     = now.minusSeconds(40L * 86400)
        val ranking = InvestmentRankingRun(run.rankingId, old, run.recipe, run.inputs, run.scorecards)
        for
          repo    <- InvestmentRankingRepository.make[IO](db)
          inputs  <- InvestmentRankingInputRepository.make[IO](db)
          screens <- InvestmentScreenRepository.make[IO](db)
          service <- rankingService(db)
          published = List(
            ranking,
            ranking.copy(id = InvestmentRankingId("obsolete")),
            ranking.copy(id = InvestmentRankingId("recent"), createdAt = now)
          )
          _ <- published.traverse { value =>
            inputs.save(value).flatMap(_ => repo.save(InvestmentRankingHeader.from(value)))
          }
          _         <- screens.save(InvestmentScreenHeader.from(run))
          _         <- inputs.save(ranking.copy(id = InvestmentRankingId("interrupted")))
          _         <- inputs.save(ranking.copy(id = InvestmentRankingId("in-flight"), createdAt = now))
          _         <- inputs.save(ranking.copy(id = InvestmentRankingId("legacy-orphan")))
          rawInputs <- db.getCollectionWithCodec[entities.InvestmentRankingInputEntity](InvestmentRankingInputRepository.CollectionName)
          _         <- rawInputs.updateOne(Filter.eq("rankingId", "legacy-orphan"), Update.unset("createdAt"))
          _         <- service.prune(now.minusSeconds(30L * 86400))
          retained  <- service.get(run.rankingId)
          obsolete  <- repo.find(InvestmentRankingId("obsolete"))
          recent    <- service.get(InvestmentRankingId("recent"))
          rows      <- rawInputs.find.all
        yield
          retained mustBe ranking
          obsolete mustBe None
          recent.id mustBe InvestmentRankingId("recent")
          rows.map(_.rankingId).toSet mustBe Set(run.rankingId, InvestmentRankingId("recent"), InvestmentRankingId("in-flight"))
      }
  }

  "An EvaluationCapturePlanRepository" should {
    "retain the original monthly price cutoff when deferred work resumes on a later date" in
      withEmbeddedMongoDatabase { db =>
        val original = EvaluationCapturePlan(run.id, "2026-10", LocalDate.parse("2026-10-20"), now)
        for
          repo   <- EvaluationCapturePlanRepository.make[IO](db)
          first  <- repo.getOrCreate(original)
          later  <- repo.getOrCreate(original.copy(through = LocalDate.parse("2026-10-25")))
          stored <- repo.find(run.id, "2026-10")
          all    <- repo.all
          raw    <- db.getCollection(EvaluationCapturePlanRepository.CollectionName)
          plan   <- raw.find(Filter.idEq(s"${run.id.value}:2026-10") && Filter.eq("plan.screenId", run.id.value)).first
        yield
          first mustBe original
          later mustBe original
          stored mustBe Some(original)
          all mustBe List(original)
          plan must not be empty
      }
    "retire only the requested screen and month" in
      withEmbeddedMongoDatabase { db =>
        val october  = EvaluationCapturePlan(run.id, "2026-10", LocalDate.parse("2026-09-30"), now)
        val november = october.copy(month = "2026-11", through = LocalDate.parse("2026-10-31"))
        for
          repo      <- EvaluationCapturePlanRepository.make[IO](db)
          _         <- repo.getOrCreate(october)
          _         <- repo.getOrCreate(november)
          _         <- repo.retire(run.id, october.month)
          remaining <- repo.all
        yield remaining mustBe List(november)
      }
  }
