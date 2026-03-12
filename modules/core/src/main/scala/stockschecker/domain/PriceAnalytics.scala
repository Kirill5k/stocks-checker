package stockschecker.domain

import cats.data.NonEmptyList
import io.circe.Codec as CirceCodec

import java.time.LocalDate
import scala.math.BigDecimal.RoundingMode

final case class PricePerformanceSummary(
    latestPrice: BigDecimal,
    latestPriceDate: LocalDate,
    oneMonthChange: Option[BigDecimal],
    threeMonthChange: Option[BigDecimal],
    sixMonthChange: Option[BigDecimal],
    oneYearChange: Option[BigDecimal],
    threeYearChange: Option[BigDecimal],
    fiveYearChange: Option[BigDecimal],
    tenYearChange: Option[BigDecimal],
    maxChange: Option[BigDecimal]
) derives CirceCodec.AsObject

final case class StockAnalysisMetrics(
    cagr3Year: Option[BigDecimal],
    cagr5Year: Option[BigDecimal],
    volatility: Option[BigDecimal],
    maxDrawdown: Option[BigDecimal],
    consistencyScore: Option[BigDecimal],
    positiveYears: Int,
    totalYears: Int
) derives CirceCodec.AsObject

final case class StockAnalysisScores(
    overallScore: BigDecimal,
    cagrScore: BigDecimal,
    volatilityScore: BigDecimal,
    drawdownScore: BigDecimal,
    consistencyScore: BigDecimal
) derives CirceCodec.AsObject

final case class PriceAnalytics(
    ticker: Ticker,
    performanceSummary: PricePerformanceSummary,
    metrics: StockAnalysisMetrics,
    scores: StockAnalysisScores
) derives CirceCodec.AsObject

object PriceAnalytics:
  def from(ticker: Ticker, priceCandles: NonEmptyList[PriceCandle]): PriceAnalytics = {
    val metrics = calculateMetrics(priceCandles)
    PriceAnalytics(
      ticker = ticker,
      performanceSummary = calculatePerformanceSummary(priceCandles),
      metrics = metrics,
      scores = calculateScores(metrics)
    )
  }

  extension (pa: PriceAnalytics)
    def toLatestPrice: LatestPrice =
      LatestPrice(pa.ticker, pa.performanceSummary.latestPrice, pa.performanceSummary.latestPriceDate)

  private def calculatePerformanceSummary(priceCandles: NonEmptyList[PriceCandle]): PricePerformanceSummary = {
    val latestCandle    = priceCandles.head
    val latestPrice     = latestCandle.close
    val latestPriceDate = latestCandle.date

    def calculateChange(prevCandle: Option[PriceCandle]): Option[BigDecimal] =
      prevCandle.flatMap { lastCandle =>
        val earliestPrice = lastCandle.close
        Option.when(earliestPrice > 0) {
          val change = (latestPrice - earliestPrice) / earliestPrice * 100
          change.setScale(2, RoundingMode.HALF_UP)
        }
      }

    def calculatePeriodChange(monthsAgo: Int): Option[BigDecimal] =
      calculateChange(priceCandles.toList.lift(monthsAgo))

    PricePerformanceSummary(
      latestPrice = latestPrice,
      latestPriceDate = latestPriceDate,
      oneMonthChange = calculatePeriodChange(1),
      threeMonthChange = calculatePeriodChange(3),
      sixMonthChange = calculatePeriodChange(6),
      oneYearChange = calculatePeriodChange(12),
      threeYearChange = calculatePeriodChange(36),
      fiveYearChange = calculatePeriodChange(60),
      tenYearChange = calculatePeriodChange(120),
      maxChange = calculateChange(priceCandles.tail.lastOption)
    )
  }

  private def calculateMetrics(candles: NonEmptyList[PriceCandle]): StockAnalysisMetrics = {
    val latestPrice = candles.head.close
    val prices      = candles.map(_.close).toList

    val yearlyReturns = calculateYearlyReturns(prices)
    val positiveYears = yearlyReturns.count(_ > 0)
    val totalYears    = yearlyReturns.size

    StockAnalysisMetrics(
      cagr3Year = if (candles.size >= 37) Some(calculateCAGR(prices(36), latestPrice, 3)) else None,
      cagr5Year = if (candles.size >= 61) Some(calculateCAGR(prices(60), latestPrice, 5)) else None,
      volatility = calculateVolatility(prices),
      maxDrawdown = calculateMaxDrawdown(prices),
      consistencyScore = calculateConsistency(candles),
      positiveYears = positiveYears,
      totalYears = totalYears
    )
  }

  private def calculateCAGR(startPrice: BigDecimal, endPrice: BigDecimal, years: Int): BigDecimal =
    if (startPrice <= 0 || years <= 0) BigDecimal(0)
    else {
      val ratio = endPrice / startPrice
      if (ratio <= 0) BigDecimal(0)
      else {
        val cagr = (Math.pow(ratio.toDouble, 1.0 / years.toDouble) - 1.0) * 100.0
        BigDecimal(cagr).setScale(2, RoundingMode.HALF_UP)
      }
    }

  private def calculateVolatility(prices: List[BigDecimal]): Option[BigDecimal] = {
    // Need at least 13 months for 12 monthly returns
    // Calculate monthly returns
    val returns =
      if (prices.size < 13) Nil
      else
        prices
          .sliding(2)
          .flatMap {
            case List(newer, older) =>
              if (older <= 0) Some(0.0)
              else Some(((newer - older) / older * 100).toDouble)
            case _ => None
          }
          .toList

    Option.when(returns.nonEmpty) {
      val mean     = returns.sum / returns.size
      val variance = returns.map(r => Math.pow(r - mean, 2)).sum / (returns.size - 1)
      val stdDev   = Math.sqrt(variance)
      // Annualize the volatility (monthly to annual)
      val annualizedVolatility = stdDev * Math.sqrt(12)
      BigDecimal(annualizedVolatility).setScale(2, RoundingMode.HALF_UP)
    }
  }

  private def calculateMaxDrawdown(prices: List[BigDecimal]): Option[BigDecimal] =
    Option.when(prices.nonEmpty) {
      val chronologicalPrices = prices.reverse
      var maxDrawdown         = BigDecimal(0)
      var peak                = chronologicalPrices.head

      chronologicalPrices.foreach { price =>
        if (price > peak) peak = price
        if (peak > 0) {
          val drawdown = ((peak - price) / peak * 100).setScale(2, RoundingMode.HALF_UP)
          if (drawdown > maxDrawdown) maxDrawdown = drawdown
        }
      }

      maxDrawdown
    }

  private def calculateConsistency(candles: NonEmptyList[PriceCandle]): Option[BigDecimal] = {
    val dataPoints = candles.toList.zipWithIndex.map { case (candle, idx) =>
      (idx.toDouble, candle.close.toDouble)
    }

    Option.when(dataPoints.size >= 3) {
      val n     = dataPoints.size
      val sumX  = dataPoints.map(_._1).sum
      val sumY  = dataPoints.map(_._2).sum
      val sumXY = dataPoints.map { case (x, y) => x * y }.sum
      val sumX2 = dataPoints.map { case (x, _) => x * x }.sum

      val meanX = sumX / n
      val meanY = sumY / n

      val ssTotal = dataPoints.map { case (_, y) => Math.pow(y - meanY, 2) }.sum
      val ssRes   = dataPoints.map { case (x, y) =>
        val yPred = meanY + (sumXY - n * meanX * meanY) / (sumX2 - n * meanX * meanX) * (x - meanX)
        Math.pow(y - yPred, 2)
      }.sum

      val rSquared = if (ssTotal == 0) 0.0 else 1.0 - (ssRes / ssTotal)
      BigDecimal(Math.max(0.0, Math.min(1.0, rSquared))).setScale(4, RoundingMode.HALF_UP)
    }
  }

  private def calculateYearlyReturns(prices: List[BigDecimal]): List[BigDecimal] =
    // Calculate year-over-year returns (12 months apart)
    prices
      .sliding(13, 12)
      .flatMap { window =>
        if (window.size == 13) {
          val startPrice = window.last // oldest in this window
          val endPrice   = window.head // newest
          if (startPrice <= 0) None
          else Some(((endPrice - startPrice) / startPrice * 100).setScale(2, RoundingMode.HALF_UP))
        } else None
      }
      .toList

  private def calculateScores(metrics: StockAnalysisMetrics): StockAnalysisScores = {
    val blendedCagr = (metrics.cagr5Year, metrics.cagr3Year) match
      case (Some(c5), Some(c3)) => Some((c5 * 0.6 + c3 * 0.4).setScale(2, RoundingMode.HALF_UP))
      case (Some(c5), None)     => Some(c5)
      case (None, Some(c3))     => Some(c3)
      case _                    => None
    val cagrScore        = scoreCAGR(blendedCagr)
    val volatilityScore  = scoreVolatility(metrics.volatility)
    val drawdownScore    = scoreDrawdown(metrics.maxDrawdown)
    val consistencyScore = scoreConsistency(metrics.consistencyScore, metrics.positiveYears, metrics.totalYears)

    val overallScore = calculateOverallScore(cagrScore, volatilityScore, drawdownScore, consistencyScore)

    StockAnalysisScores(
      overallScore = overallScore,
      cagrScore = cagrScore,
      volatilityScore = volatilityScore,
      drawdownScore = drawdownScore,
      consistencyScore = consistencyScore
    )
  }

  private def scoreCAGR(cagr: Option[BigDecimal]): BigDecimal =
    cagr match
      case None               => BigDecimal(0)
      case Some(c) if c < 0   => BigDecimal(0)
      case Some(c) if c >= 30 => BigDecimal(100)
      case Some(c)            => (c / 30 * 100).setScale(2, RoundingMode.HALF_UP)

  private def scoreVolatility(volatility: Option[BigDecimal]): BigDecimal =
    volatility match
      case None               => BigDecimal(0)
      case Some(v) if v >= 35 => BigDecimal(0)
      case Some(v) if v <= 10 => BigDecimal(100)
      case Some(v)            => ((35 - v) / 25 * 100).setScale(2, RoundingMode.HALF_UP)

  private def scoreDrawdown(drawdown: Option[BigDecimal]): BigDecimal =
    drawdown match
      case None               => BigDecimal(0)
      case Some(d) if d >= 60 => BigDecimal(0)
      case Some(d) if d <= 15 => BigDecimal(100)
      case Some(d)            => ((60 - d) / 45 * 100).setScale(2, RoundingMode.HALF_UP)

  private def scoreConsistency(rSquared: Option[BigDecimal], positiveYears: Int, totalYears: Int): BigDecimal = {
    val r2Score            = rSquared.map(_ * 100).getOrElse(BigDecimal(0)).setScale(2, RoundingMode.HALF_UP)
    val positiveYearsScore =
      if (totalYears == 0) BigDecimal(0)
      else BigDecimal(positiveYears.toDouble / totalYears.toDouble * 100).setScale(2, RoundingMode.HALF_UP)

    ((r2Score + positiveYearsScore) / 2).setScale(2, RoundingMode.HALF_UP)
  }

  private def calculateOverallScore(
      cagrScore: BigDecimal,
      volatilityScore: BigDecimal,
      drawdownScore: BigDecimal,
      consistencyScore: BigDecimal
  ): BigDecimal = {
    val weighted =
      cagrScore * 0.30 +
        volatilityScore * 0.25 +
        drawdownScore * 0.20 +
        consistencyScore * 0.25
    weighted.setScale(2, RoundingMode.HALF_UP)
  }
