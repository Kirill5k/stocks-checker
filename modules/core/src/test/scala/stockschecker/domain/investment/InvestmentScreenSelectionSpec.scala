package stockschecker.domain.investment

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import stockschecker.domain.*
import stockschecker.fixtures.*

import java.time.LocalDate

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
    cards.map(c =>
      InvestmentRankingEntry.from(InvestmentCandidate(AAPLStock.copy(security = AAPLSecurity.copy(ticker = c.ticker)), None), c)
    )

  private def screen(
      values: List[InvestmentRankingEntry],
      request: InvestmentScreenRequest,
      cohorts: List[InvestmentCohort]
  ): InvestmentScreen =
    val ranking = InvestmentRanking(InvestmentRankingId("published"), ts, InvestmentRecipe(), InvestmentRankingSummary.from(values))
    InvestmentScreen.create(InvestmentScreenId("selection"), ts, ranking, request, cohorts)

  "InvestmentScreenSelection" should {
    "preserve scope, cohort ordering and financial explanations after compacting full candidates" in {
      val excludedTicker = Ticker("NEGATIVE")
      val outsideTicker  = Ticker("OUTSIDE")
      val financial      = FinancialSnapshot(
        "negative-pe",
        excludedTicker,
        Provider.Finnhub,
        ts,
        Map(FinancialMetric.PriceToEarningsTtm -> BigDecimal(-3)),
        List(FinancialObservation(FinancialMetric.ReturnOnAssets, LocalDate.parse("2025-12-31"), BigDecimal("0.2"))),
        Nil,
        "payload-hash",
        "mapping-v1",
        Nil
      )
      def candidate(ticker: Ticker, country: String, priceScore: Int): InvestmentCandidate =
        InvestmentCandidate(
          AAPLStock.copy(
            security = AAPLSecurity.copy(ticker = ticker),
            profile =
              Some(AAPLCompanyProfile.copy(ticker = ticker, country = country, industry = "Software", sector = Some(Sector.Technology))),
            priceAnalytics =
              Some(AAPLPriceAnalytics.copy(ticker = ticker, scores = AAPLPriceAnalytics.scores.copy(overallScore = priceScore)))
          ),
          Option.when(ticker == excludedTicker)(financial)
        )
      val selected = card(AAPL, 90).copy(pillars = Map(Pillar.Quality -> BigDecimal(50), Pillar.Value -> BigDecimal(80)))
      val nearMiss = card(MSFT, 60).copy(pillars = Map(Pillar.Quality -> BigDecimal(90), Pillar.Value -> BigDecimal(40)))
      val excluded = card(excludedTicker, 0).copy(
        eligible = false,
        score = None,
        exclusions = List(InvestmentReasons.nonpositiveMetric(FinancialMetric.PriceToEarningsTtm.print)),
        financialSnapshotId = Some(financial.id)
      )
      val full = List(
        candidate(AAPL, "US", 10)          -> selected,
        candidate(MSFT, "US", 90)          -> nearMiss,
        candidate(excludedTicker, "US", 5) -> excluded,
        candidate(outsideTicker, "GB", 99) -> card(outsideTicker, 99)
      )
      val values    = full.map { case (input, scorecard) => InvestmentRankingEntry.from(input, scorecard) }
      val request   = InvestmentScreenRequest(exchange = Some(Exchange.NASDAQ), country = Some("US"), minScore = Some(70))
      val selection = InvestmentScreenSelection(values, request)
      val result    = selection.result(screen(values, request, selection.cohorts))

      values.map(_.scorecard) mustBe full.map(_._2)
      result.candidates mustBe List(selected)
      result.nearMisses mustBe List(
        InvestmentNearMiss(nearMiss, "score 60 is below required score 70"),
        InvestmentNearMiss(excluded, s"${FinancialMetric.PriceToEarningsTtm.print} = -3; requires a positive value")
      )
      result.exclusions mustBe List(excluded)
      result.cohorts.map(c => c.name -> c.tickers).toMap mustBe Map(
        CohortName.Investment    -> List(AAPL),
        CohortName.OldPriceScore -> List(MSFT, AAPL),
        CohortName.Quality       -> List(MSFT, AAPL),
        CohortName.Value         -> List(AAPL, MSFT),
        CohortName.EqualUniverse -> List(AAPL, MSFT)
      )
      result.cohorts.foreach { cohort =>
        cohort.concentration.map(_.sectorWeightsPercent) mustBe Some(Map("technology" -> BigDecimal(100)))
        cohort.concentration.map(_.industryWeightsPercent) mustBe Some(Map("Software" -> BigDecimal(100)))
      }
    }

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
        entry.copy(input = entry.input.copy(industry = Some("Communications"), sector = Some(Sector.CommunicationServices)))
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
