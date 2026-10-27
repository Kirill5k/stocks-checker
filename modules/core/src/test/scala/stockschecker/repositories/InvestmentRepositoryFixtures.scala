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
    Map("quality" -> BigDecimal(80)),
    Map.empty,
    Nil,
    Nil,
    Some("industry:test"),
    20,
    None,
    "fundamental-v1"
  )
  val screenRun = InvestmentScreenRun(
    InvestmentScreenId("immutable-run"),
    now,
    InvestmentRecipe(),
    InvestmentScreenRequest(),
    List(InvestmentCandidate(AAPLStock, None)),
    List(card),
    List(InvestmentCohort("investment", List(AAPL))),
    InvestmentRankingId("rank-1")
  )

}
