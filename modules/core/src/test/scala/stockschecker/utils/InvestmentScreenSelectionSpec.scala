package stockschecker.utils

import stockschecker.domain.*

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import stockschecker.fixtures.*

class InvestmentScreenSelectionSpec extends AnyWordSpec with Matchers:
  private def card(ticker: Ticker, score: Int): InvestmentScorecard =
    InvestmentScorecard(
      ticker,
      true,
      Some(BigDecimal(score)),
      Map("quality" -> BigDecimal(90)),
      Map.empty,
      Nil,
      Nil,
      Some("peer"),
      25,
      None,
      "v1"
    )

  private def run(cards: List[InvestmentScorecard], request: InvestmentScreenRequest): InvestmentScreenRun =
    InvestmentScreenRun(
      InvestmentScreenId("selection"),
      ts,
      InvestmentRecipe(),
      request,
      cards.map(c => InvestmentCandidate(AAPLStock.copy(security = AAPLSecurity.copy(ticker = c.ticker)), None)),
      cards,
      Nil,
      InvestmentRankingId("published")
    )

  "InvestmentScreenSelection" should {
    "apply the same score criteria to displayed candidates and the fixed top-twenty cohort" in {
      val cards     = (0 until 25).toList.map(n => card(Ticker(f"TEST$n%02d"), 100 - n))
      val screen    = run(cards, InvestmentScreenRequest(limit = Some(1), minScore = Some(80)))
      val selection = InvestmentScreenSelection(screen.inputs, screen.scorecards, screen.request)
      val result    = selection.result(screen.copy(cohorts = selection.cohorts))

      result.candidates.map(_.ticker) mustBe List(Ticker("TEST00"))
      result.cohorts.find(_.name == "investment").get.tickers mustBe cards.take(20).map(_.ticker)
      result.cohorts.find(_.name == "equal-universe").get.tickers mustBe cards.map(_.ticker)
      result.nearMisses.map(_.scorecard.ticker) mustBe cards.drop(21).map(_.ticker)
      result.summary.universeCount mustBe 25
      result.summary.eligibleCount mustBe 25
      result.summary.selectedCount mustBe 20
      val concentration = result.cohorts.find(_.name == "investment").get.concentration.get
      concentration.maxPositionWeightPercent mustBe BigDecimal(5)
      concentration.positionHerfindahl mustBe BigDecimal("0.05")
    }

    "preserve frozen cohorts when rebuilding a screen result" in {
      val frozen = List(InvestmentCohort("investment", List(MSFT)))
      val screen = run(List(card(AAPL, 90), card(MSFT, 80)), InvestmentScreenRequest()).copy(cohorts = frozen)
      val result = InvestmentScreenSelection(screen.inputs, screen.scorecards, screen.request).result(screen)

      result.candidates.map(_.ticker) mustBe List(AAPL, MSFT)
      result.cohorts mustBe frozen
      result.summary.selectedCount mustBe 1
    }

    "treat a missing pillar as one failed criterion and omit candidates failing two" in {
      val cards  = List(card(AAPL, 90), card(MSFT, 90).copy(pillars = Map.empty), card(Ticker("TWO"), 60).copy(pillars = Map.empty))
      val screen = run(cards, InvestmentScreenRequest(minScore = Some(70), minQuality = Some(70)))
      val result = InvestmentScreenSelection(screen.inputs, screen.scorecards, screen.request).result(screen)

      result.candidates.map(_.ticker) mustBe List(AAPL)
      result.nearMisses.map(_.scorecard.ticker) mustBe List(MSFT)
      result.nearMisses.head.failedRule mustBe "quality missing is below required quality 70"
    }
  }
