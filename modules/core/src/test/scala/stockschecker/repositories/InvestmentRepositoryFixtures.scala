package stockschecker.repositories

import cats.effect.IO
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Filter, Sort}
import stockschecker.repositories.entities.{InvestmentRankingEntity, InvestmentRankingInputEntity}
import stockschecker.domain.*
import stockschecker.fixtures.*

import java.time.Instant

private[stockschecker] object InvestmentRepositoryFixtures {
  def storedRanking(db: MongoDatabase[IO], id: InvestmentRankingId): IO[Option[InvestmentRanking]] =
    db.getCollectionWithCodec[InvestmentRankingEntity](InvestmentRankingRepository.CollectionName)
      .flatMap(_.find(Filter.idEq(id.value)).first)
      .map(_.map(_.toDomain))

  def storedRankingEntries(db: MongoDatabase[IO], id: InvestmentRankingId): IO[List[InvestmentRankingEntry]] =
    db.getCollectionWithCodec[InvestmentRankingInputEntity](InvestmentRankingRepository.InputsCollectionName)
      .flatMap(_.find(Filter.eq("rankingId", id.value)).sort(Sort.asc("ordinal")).all)
      .map(_.toList.map(row => InvestmentRankingEntry(row.input, row.scorecard)))

  val now  = Instant.parse("2026-10-19T12:00:00Z")
  val card = InvestmentScorecard(
    AAPL,
    true,
    Some(BigDecimal(70)),
    Map(Pillar.Quality -> BigDecimal(80)),
    Map.empty,
    Nil,
    Nil,
    Some("industry:test"),
    20,
    None,
    "fundamental-v1"
  )
  val entries = List(InvestmentRankingEntry.from(InvestmentCandidate(AAPLStock, None), card))
  val ranking = InvestmentRanking(InvestmentRankingId("rank-1"), now, InvestmentRecipe(), InvestmentRankingSummary.from(entries))
  val screen  = InvestmentScreen.create(
    InvestmentScreenId("immutable-run"),
    now,
    ranking,
    InvestmentScreenRequest(),
    List(InvestmentCohort(CohortName.Investment, List(AAPL)))
  )
}
