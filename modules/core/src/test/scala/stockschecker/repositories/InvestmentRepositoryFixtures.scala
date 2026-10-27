package stockschecker.repositories

import stockschecker.domain.*
import stockschecker.fixtures.*

import java.time.Instant

private[stockschecker] object InvestmentRepositoryFixtures {
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
  val entries = List(InvestmentRankingEntry(InvestmentCandidate(AAPLStock, None), card))
  val ranking = InvestmentRanking(InvestmentRankingId("rank-1"), now, InvestmentRecipe(), InvestmentRankingSummary.from(entries))
  val screen  = InvestmentScreen.create(
    InvestmentScreenId("immutable-run"),
    now,
    ranking,
    InvestmentScreenRequest(),
    List(InvestmentCohort(CohortName.Investment, List(AAPL)))
  )
}
