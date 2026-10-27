package stockschecker.domain.investment

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import stockschecker.domain.*
import stockschecker.fixtures.*

class InvestmentScreenSelectionSpec extends AnyWordSpec with Matchers:
  private def card(ticker: Ticker, score: Int): InvestmentScorecard =
    InvestmentScorecard(
      ticker,
      true,
      Some(BigDecimal(score)),
      Map(Pillar.Quality -> BigDecimal(90)),
      Map.empty,
      Nil,
      Nil,
      Some("peer"),
      25,
      None,
      "v1"
    )

  private def entries(cards: List[InvestmentScorecard]): List[InvestmentRankingEntry] =
    cards.map(c => InvestmentRankingEntry(InvestmentCandidate(AAPLStock.copy(security = AAPLSecurity.copy(ticker = c.ticker)), None), c))

  private def screen(
      values: List[InvestmentRankingEntry],
      request: InvestmentScreenRequest,
      cohorts: List[InvestmentCohort]
  ): InvestmentScreen =
    val ranking = InvestmentRanking(InvestmentRankingId("published"), ts, InvestmentRecipe(), InvestmentRankingSummary.from(values))
    InvestmentScreen.create(InvestmentScreenId("selection"), ts, ranking, request, cohorts)

  "InvestmentScreenSelection" should {
    "apply the same score criteria to displayed candidates and the fixed top-twenty cohort" in {
      val cards     = (0 until 25).toList.map(n => card(Ticker(f"TEST$n%02d"), 100 - n))
      val values    = entries(cards)
      val request   = InvestmentScreenRequest(limit = Some(1), minScore = Some(80))
      val selection = InvestmentScreenSelection(values, request)
      val result    = selection.result(screen(values, request, selection.cohorts))

      result.candidates.map(_.ticker) mustBe List(Ticker("TEST00"))
      result.cohorts.find(_.name == CohortName.Investment).get.tickers mustBe cards.take(20).map(_.ticker)
      result.cohorts.find(_.name == CohortName.EqualUniverse).get.tickers mustBe cards.map(_.ticker)
      result.nearMisses.map(_.scorecard.ticker) mustBe cards.drop(21).map(_.ticker)
      result.summary.universeCount mustBe 25
      result.summary.eligibleCount mustBe 25
      result.summary.selectedCount mustBe 20
      val concentration = result.cohorts.find(_.name == CohortName.Investment).get.concentration.get
      concentration.maxPositionWeightPercent mustBe BigDecimal(5)
      concentration.positionHerfindahl mustBe BigDecimal("0.05")
    }

    "preserve the stored summary and frozen cohorts when rebuilding a screen result" in {
      val frozen = List(InvestmentCohort(CohortName.Investment, List(MSFT)))
      val values = entries(List(card(AAPL, 90), card(MSFT, 80)))
      val saved  = screen(values, InvestmentScreenRequest(), frozen)
      val result = InvestmentScreenSelection(values, saved.request).result(saved)

      result.candidates.map(_.ticker) mustBe List(AAPL, MSFT)
      result.cohorts mustBe frozen
      result.summary mustBe saved.summary
      result.summary.selectedCount mustBe 1
    }

    List(
      (Pillar.Quality, InvestmentScreenRequest(minQuality = Some(70))),
      (Pillar.Value, InvestmentScreenRequest(minValue = Some(70))),
      (Pillar.Growth, InvestmentScreenRequest(minGrowth = Some(70))),
      (Pillar.Resilience, InvestmentScreenRequest(minResilience = Some(70)))
    ).foreach { case (pillar, request) =>
      s"report the ${pillar.print} criterion when its score is below the threshold" in {
        val values = entries(List(card(AAPL, 90).copy(pillars = Map(pillar -> BigDecimal(60)))))
        val result = InvestmentScreenSelection(values, request).result(screen(values, request, Nil))

        result.candidates mustBe Nil
        result.nearMisses.map(_.failedRule) mustBe List(s"${pillar.print} 60 is below required ${pillar.print} 70")
      }
    }

    "preserve sector labels when calculating concentration weights" in {
      val values = entries(List(card(AAPL, 90))).map { entry =>
        entry.copy(candidate =
          entry.candidate.copy(stock =
            entry.candidate.stock.copy(
              profile = entry.candidate.stock.profile.map(_.copy(industry = "Communications", sector = Some(Sector.CommunicationServices)))
            )
          )
        )
      }
      val selection     = InvestmentScreenSelection(values, InvestmentScreenRequest())
      val concentration = selection.cohorts.find(_.name == CohortName.Investment).flatMap(_.concentration).get

      concentration.sectorWeightsPercent mustBe Map("communication-services" -> BigDecimal(100))
    }

    "treat a missing pillar as one failed criterion and omit candidates failing two" in {
      val values =
        entries(List(card(AAPL, 90), card(MSFT, 90).copy(pillars = Map.empty), card(Ticker("TWO"), 60).copy(pillars = Map.empty)))
      val request = InvestmentScreenRequest(minScore = Some(70), minQuality = Some(70))
      val result  = InvestmentScreenSelection(values, request).result(screen(values, request, Nil))

      result.candidates.map(_.ticker) mustBe List(AAPL)
      result.nearMisses.map(_.scorecard.ticker) mustBe List(MSFT)
      result.nearMisses.head.failedRule mustBe "quality missing is below required quality 70"
    }
  }
