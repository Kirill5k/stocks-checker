package stockschecker.repositories

import cats.effect.IO
import mongo4cats.operations.{Filter, Update}
import stockschecker.domain.*
import stockschecker.repositories.InvestmentRepositoryFixtures.*

class InvestmentRankingRepositorySpec extends RepositorySpec:
  override val port = 12223

  "An InvestmentRankingRepository" should {
    "preserve legacy missing summaries without querying ranking inputs" in
      withEmbeddedMongoDatabase { db =>
        val ranking =
          InvestmentRankingRun(InvestmentRankingId("legacy-metadata"), now, screenRun.recipe, screenRun.inputs, screenRun.scorecards)
        val header = InvestmentRankingHeader.from(ranking)
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
