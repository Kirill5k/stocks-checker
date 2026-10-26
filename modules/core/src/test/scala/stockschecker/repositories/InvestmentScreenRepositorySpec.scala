package stockschecker.repositories

import cats.effect.IO
import stockschecker.domain.*
import stockschecker.fixtures.*
import java.time.{Instant, LocalDate}

class InvestmentScreenRepositorySpec extends RepositorySpec:
  val port = 12161
  private val now = Instant.parse("2026-10-19T12:00:00Z")
  private val card = InvestmentScorecard(AAPL, true, Some(BigDecimal(70)), Map("quality" -> BigDecimal(80)), Map.empty, Nil, Nil, Some("industry:test"), 20, None, "fundamental-v1")
  private val run = InvestmentScreenRun("immutable-run", now, InvestmentRecipe(), InvestmentScreenRequest(),
    List(InvestmentCandidate(AAPLStock, None)), List(card), List(InvestmentCohort("investment", List(AAPL))), "rank-1")

  "An InvestmentScreenRepository" should {
    "round trip the frozen input universe and reject overwriting a published screen" in {
      withEmbeddedMongoDatabase { db =>
        for
          repo <- InvestmentScreenRepository.make[IO](db)
          ranks <- InvestmentRankingRepository.make[IO](db)
          _ <- ranks.save(InvestmentRankingRun(run.rankingId, now, run.recipe, run.inputs, run.scorecards))
          _ <- repo.save(run)
          overwrite <- repo.save(run.copy(scorecards = List(card.copy(score = Some(BigDecimal(99)))))).attempt
          saved <- repo.find(run.id)
          listed <- repo.list(20)
        yield
          overwrite.isLeft mustBe true
          saved mustBe Some(run)
          listed.map(_.id) mustBe List(run.id)
      }
    }
    "not expose an input batch without its completed screen header" in {
      withEmbeddedMongoDatabase { db =>
        for
          repo <- InvestmentScreenRepository.make[IO](db)
          saved <- repo.find("unfinished")
          listed <- repo.list(20)
        yield
          saved mustBe None
          listed mustBe Nil
      }
    }
    "save a single immutable evaluation for a screen and month" in {
      withEmbeddedMongoDatabase { db =>
        val evaluation = InvestmentEvaluator.evaluate(run.id, now, now, run.cohorts, Map.empty)
        for
          repo <- InvestmentEvaluationRepository.make[IO](db)
          _ <- repo.saveMonthly(evaluation)
          overwrite <- repo.saveMonthly(evaluation.copy(methodology = "changed")).attempt
          saved <- repo.findMonthly(run.id, "2026-10")
        yield
          overwrite.isLeft mustBe true
          saved mustBe Some(evaluation)
      }
    }
    "retain the original monthly price cutoff when deferred work resumes on a later date" in {
      withEmbeddedMongoDatabase { db =>
        val original = EvaluationCapturePlan(run.id, "2026-10", LocalDate.parse("2026-10-20"), now)
        for
          repo <- InvestmentEvaluationRepository.make[IO](db)
          first <- repo.getOrCreateCapturePlan(original)
          later <- repo.getOrCreateCapturePlan(original.copy(through = LocalDate.parse("2026-10-25")))
          stored <- repo.findCapturePlan(run.id, "2026-10")
        yield
          first mustBe original
          later mustBe original
          stored mustBe Some(original)
      }
    }
  }
