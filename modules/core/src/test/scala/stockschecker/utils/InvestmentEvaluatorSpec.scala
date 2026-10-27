package stockschecker.utils

import stockschecker.domain.*

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.time.{Instant, LocalDate}

class InvestmentEvaluatorSpec extends AnyWordSpec with Matchers:
  private val formed  = Instant.parse("2020-01-01T23:00:00Z")
  private val now     = Instant.parse("2025-01-03T00:00:00Z")
  private val dates   = List("2020-01-01", "2020-01-02", "2021-01-04", "2023-01-02", "2025-01-02").map(LocalDate.parse)
  private val tickerA = Ticker("AAA")
  private val b       = Ticker("BBB")
  private def series(ticker: Ticker, prices: List[Int]): EvaluationPriceSeries =
    EvaluationPriceSeries(
      ticker,
      dates.zip(prices).map((date, price) => EvaluationClose(date, BigDecimal(price))),
      "audited-test",
      now,
      true,
      "split-and-dividend",
      DateRange(dates.head, dates.last)
    )
  private val vti = series(InvestmentEvaluator.Benchmark, List(1, 100, 110, 120, 130))
  private def evaluate(cohort: List[Ticker], histories: Map[Ticker, EvaluationPriceSeries]): InvestmentEvaluation =
    InvestmentEvaluator.evaluate(
      InvestmentScreenId("run"),
      formed,
      now,
      List(InvestmentCohort("investment", cohort)),
      histories + (InvestmentEvaluator.Benchmark -> vti)
    )

  "Forward investment evaluation" should {
    "enter on the next session and calculate weighted wealth, drawdown, benchmark excess and costs" in {
      val report  = evaluate(List(tickerA), Map(tickerA -> series(tickerA, List(1000, 100, 50, 150, 200))))
      val horizon = report.horizons.head
      val result  = horizon.cohorts.find(_.name == "investment").get
      horizon.entryDate mustBe Some(LocalDate.parse("2020-01-02"))
      result.totalReturnPercent mustBe Some(BigDecimal(50))
      result.maxDrawdownPercent mustBe Some(BigDecimal(50))
      result.excessReturnPercent mustBe Some(BigDecimal(30))
      result.costs.find(_.roundTripBasisPoints == 100).get.totalReturnPercent mustBe BigDecimal(49)
      result.annualizedReturnPercent.get.toDouble mustBe 14.4714 +- 0.0001
    }
    "retain losing constituents at their original equal weights" in {
      val report = evaluate(
        List(tickerA, b),
        Map(tickerA -> series(tickerA, List(100, 100, 100, 150, 200)), b -> series(b, List(100, 100, 100, 50, 50)))
      )
      report.horizons.head.cohorts.find(_.name == "investment").get.totalReturnPercent mustBe Some(BigDecimal(0))
    }
    "mark tickerA portfolio incomplete rather than dropping tickerA missing constituent" in {
      val report = evaluate(List(tickerA, b), Map(tickerA -> series(tickerA, List(100, 100, 100, 150, 200))))
      val result = report.horizons.head.cohorts.find(_.name == "investment").get
      result.status mustBe "incomplete"
      result.missing mustBe List(b)
      result.totalReturnPercent mustBe None
      result.constituents mustBe List(tickerA, b)
    }
    "keep future horizons pending without inventing historical returns" in {
      val report =
        InvestmentEvaluator.evaluate(InvestmentScreenId("new"), now, now, List(InvestmentCohort("investment", List(tickerA))), Map.empty)
      report.horizons.map(_.status) mustBe List("pending", "pending")
      report.horizons.flatMap(_.cohorts).forall(_.totalReturnPercent.isEmpty) mustBe true
      report.horizons.head.cohorts.find(_.name == "investment").get.constituents mustBe List(tickerA)
    }
    "reject price-only, unaudited and incomplete daily data" in {
      val priceOnly = series(tickerA, List(100, 100, 100, 150, 200)).copy(adjustment = "split", audited = false)
      val result    = evaluate(List(tickerA), Map(tickerA -> priceOnly)).horizons.head.cohorts.find(_.name == "investment").get
      result.status mustBe "incomplete"
      result.issues.head must include("unaudited")
      result.issues.head must include("adjustments")
    }
    "refuse missing intermediate sessions even if entry and exit exist" in {
      val gap =
        series(tickerA, List(100, 100, 100, 150, 200)).copy(closes = List(EvaluationClose(dates(1), 100), EvaluationClose(dates(3), 150)))
      evaluate(List(tickerA), Map(tickerA -> gap)).horizons.head.cohorts.find(_.name == "investment").get.status mustBe "incomplete"
    }
  }
