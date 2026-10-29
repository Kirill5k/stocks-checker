package stockschecker.services

import cats.effect.IO
import mongo4cats.operations.{Filter, Update}
import stockschecker.domain.*
import stockschecker.domain.investment.InvestmentEvaluator
import stockschecker.repositories.{InvestmentEvaluationRepository, PriceHistoryRepository, RepositorySpec}

import java.time.{Instant, LocalDate, YearMonth, ZoneOffset}

class EvaluationPriceSeriesIntegrationSpec extends RepositorySpec:
  override val port = 12225

  private val ticker   = Ticker("AAA")
  private val from     = LocalDate.parse("2025-01-01")
  private val through  = LocalDate.parse("2025-02-01")
  private val range    = DateRange(from, through)
  private val bar      = PriceCandle(from.plusDays(1), 100, 100, 100, 100, 1L)
  private val snapshot = PriceHistorySnapshot(
    "price-vintage-1",
    ticker,
    List(bar),
    Provider.TwelveData,
    "USD",
    PriceAdjustment.SplitsAndDividends,
    Instant.parse("2025-02-02T12:00:00Z"),
    range
  )

  "Evaluation price history" should {
    "reuse a retained complete vintage with its provenance and leave cached documents unchanged" in
      withEmbeddedMongoDatabase { db =>
        for
          prices      <- PriceHistoryRepository.make[IO](db)
          evaluations <- InvestmentEvaluationRepository.make[IO](db)
          _           <- prices.save(snapshot)
          before      <- prices.latest(ticker, PriceAdjustment.SplitsAndDividends)
          found       <- evaluations.findPriceSeries(List(ticker), range)
          after       <- prices.latest(ticker, PriceAdjustment.SplitsAndDividends)
        yield
          val series = found(ticker)
          series.snapshotIds mustBe List(snapshot.id)
          series.fetchedAt mustBe snapshot.fetchedAt
          series.audited mustBe true
          series.issues mustBe Nil
          before mustBe Some(snapshot)
          after mustBe before
      }

    val truncated = snapshot.copy(
      candles = (0 until 5000).toList.map(n => bar.copy(date = through.minusDays(n.toLong))),
      requestedRange = DateRange(through.minusDays(4999), through)
    )
    List(
      snapshot.copy(currency = "EUR")                    -> "unsupported evaluation currency",
      snapshot.copy(adjustment = PriceAdjustment.Splits) -> "total-return adjustments unavailable",
      truncated                                          -> "provider response may be truncated"
    ).foreach { case (invalid, issue) =>
      s"exclude cached observations with $issue from investment returns" in
        withEmbeddedMongoDatabase { db =>
          for
            // Exercise validation even when a cache slot contains inconsistent provider adjustment metadata.
            prices <- PriceHistoryRepository.make[IO](db)
            _      <- prices.save(invalid.copy(adjustment = PriceAdjustment.SplitsAndDividends))
            raw    <- db.getCollection(PriceHistoryRepository.CollectionName)
            _      <- raw.updateOne(
              Filter.idEq(s"${ticker.value}:${PriceAdjustment.SplitsAndDividends.value}"),
              Update.set("data.adjustment", invalid.adjustment.value)
            )
            evaluations <- InvestmentEvaluationRepository.make[IO](db)
            found       <- evaluations.findPriceSeries(List(ticker), range)
            retained    <- prices.latest(ticker, PriceAdjustment.SplitsAndDividends)
          yield
            val series = found(ticker)
            series.audited mustBe false
            series.issues must contain(issue)
            val report = InvestmentEvaluator.evaluate(
              InvestmentScreenId("invalid-prices"),
              from.atStartOfDay(ZoneOffset.UTC).toInstant,
              snapshot.fetchedAt,
              List(InvestmentCohort(CohortName.Investment, List(ticker))),
              Map(ticker -> series, InvestmentEvaluator.Benchmark -> series.copy(ticker = InvestmentEvaluator.Benchmark)),
              YearMonth.from(through),
              through
            )
            report.horizons.flatMap(_.cohorts).foreach { cohort =>
              cohort.status mustBe EvaluationStatus.Incomplete
              cohort.totalReturnPercent mustBe None
              cohort.observedReturnPercent mustBe None
              cohort.issues.mkString must include(issue)
            }
            retained mustBe Some(invalid)
        }
    }
  }
