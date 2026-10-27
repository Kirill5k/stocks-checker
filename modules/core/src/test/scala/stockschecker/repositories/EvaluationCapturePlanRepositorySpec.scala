package stockschecker.repositories

import cats.effect.IO
import mongo4cats.operations.Filter
import stockschecker.domain.*

import java.time.LocalDate
import stockschecker.repositories.InvestmentRepositoryFixtures.*

class EvaluationCapturePlanRepositorySpec extends RepositorySpec:
  override val port = 12225

  "An EvaluationCapturePlanRepository" should {
    "retain the original monthly price cutoff when deferred work resumes on a later date" in
      withEmbeddedMongoDatabase { db =>
        val original = EvaluationCapturePlan(screenRun.id, "2026-10", LocalDate.parse("2026-10-20"), now)
        for
          repo   <- EvaluationCapturePlanRepository.make[IO](db)
          first  <- repo.getOrCreate(original)
          later  <- repo.getOrCreate(original.copy(through = LocalDate.parse("2026-10-25")))
          stored <- repo.find(screenRun.id, "2026-10")
          all    <- repo.all
          raw    <- db.getCollection(EvaluationCapturePlanRepository.CollectionName)
          plan   <- raw.find(Filter.idEq(s"${screenRun.id.value}:2026-10") && Filter.eq("plan.screenId", screenRun.id.value)).first
        yield
          first mustBe original
          later mustBe original
          stored mustBe Some(original)
          all mustBe List(original)
          plan must not be empty
      }
    "retire only the requested screen and month" in
      withEmbeddedMongoDatabase { db =>
        val october  = EvaluationCapturePlan(screenRun.id, "2026-10", LocalDate.parse("2026-09-30"), now)
        val november = october.copy(month = "2026-11", through = LocalDate.parse("2026-10-31"))
        for
          repo      <- EvaluationCapturePlanRepository.make[IO](db)
          _         <- repo.getOrCreate(october)
          _         <- repo.getOrCreate(november)
          _         <- repo.retire(screenRun.id, october.month)
          remaining <- repo.all
        yield remaining mustBe List(november)
      }
  }
