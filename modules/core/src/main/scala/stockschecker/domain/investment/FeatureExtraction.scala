package stockschecker.domain.investment

import stockschecker.domain.*
import kirill5k.common.syntax.time.*

import java.time.{Instant, LocalDate, ZoneOffset}
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.*

/** Accumulates independent validation failures while retaining the features that can be audited. */
object FeatureExtraction:
  final case class Failure(features: Map[String, BigDecimal], reasons: List[String])

  val Warnings: List[String] = List(
    InvestmentReasons.WeightsAreResearchHypotheses,
    InvestmentReasons.GrowthUsesProviderReportedFiveYearMetrics,
    InvestmentReasons.sectorMapping(SectorMapping.Version),
    InvestmentReasons.FiscalHistoryIsNotPointInTime,
    InvestmentReasons.PublicationTimeUnknown,
    InvestmentReasons.ReportingCurrencyUnknownNoCrossCurrencyYieldsComputed
  )

  def from(
      snapshot: FinancialSnapshot,
      ticker: Ticker,
      asOf: Instant,
      recipe: InvestmentRecipe
  ): Either[Failure, Map[String, BigDecimal]] =
    val today = asOf.atZone(ZoneOffset.UTC).toLocalDate
    val roa = history(snapshot, "roa", 5, today, recipe)
    val fcf = history(snapshot, "fcfMargin", Eligibility.CashFlowHistoryYears, today, recipe)
    val debt = history(snapshot, "totalDebtToTotalAsset", 1, today, recipe)
    val pe = Eligibility.positiveMetric("peTTM", snapshot.metrics)
    val pfcf = Eligibility.positiveMetric("pfcfShareTTM", snapshot.metrics)
    val currentEps = Eligibility.positiveMetric("epsTTM", snapshot.metrics)
    val revenueGrowth = growthMetric(snapshot, "revenueGrowth5Y")
    val epsGrowth = growthMetric(snapshot, "epsGrowth5Y")
    val alignedQuality = for
      assets <- roa
      cash <- fcf
    yield assets.map(_.period) == cash.map(_.period)
    val reasons = snapshotReasons(snapshot, ticker, asOf, recipe) ++
      List(roa, fcf).flatMap(_.left.toOption) ++
      Option.when(alignedQuality.contains(false))(InvestmentReasons.MisalignedQualityFiscalPeriods).toList ++
      List(debt, pe, pfcf, currentEps, revenueGrowth, epsGrowth).flatMap(_.left.toOption) ++
      Option.when(fcf.exists(points => !Eligibility.hasDurableCashFlow(points)))(InvestmentReasons.InsufficientCashFlowDurability).toList ++
      Option.when(debt.exists(_.exists(_.value < 0)))(InvestmentReasons.NegativeDebtToAssets).toList ++
      currentCashFlow(snapshot, today, recipe).left.toOption.toList
    val features = List(
      roa.map(points => "returnOnAssets5YMedian" -> median(points)),
      fcf.map(points => "freeCashFlowMargin5YMedian" -> median(points)),
      pe.map(value => "earningsYield" -> (BigDecimal(1) / value)),
      pfcf.map(value => "freeCashFlowYield" -> (BigDecimal(1) / value)),
      revenueGrowth.map("revenueGrowth5Y" -> _),
      epsGrowth.map("epsGrowth5Y" -> _),
      debt.map(points => "debtToAssets" -> points.head.value),
      currentEps.map("epsTtm" -> _)
    ).flatMap(_.toOption).toMap
    Either.cond(reasons.isEmpty, features, Failure(features, reasons.distinct))

  private def snapshotReasons(snapshot: FinancialSnapshot, ticker: Ticker, asOf: Instant, recipe: InvestmentRecipe): List[String] =
    val requiredSeries = Set("annual:roa", "annual:fcfMargin", "annual:totalDebtToTotalAsset", "quarterly:fcfPerShareTTM")
    val relevantErrors = snapshot.validationErrors.exists { error =>
      error.split(":", 2).toList match
        case kind :: series :: Nil if Set("invalid-series", "invalid-observation", "duplicate-period")(kind) => requiredSeries(series)
        case _ => true
    }
    List(
      Option.when(snapshot.ticker != ticker)(InvestmentReasons.FinancialTickerMismatch),
      Option.when(snapshot.provider != Provider.Finnhub || snapshot.mappingVersion != FinancialSnapshot.FinnhubMappingVersion)(
        InvestmentReasons.UnverifiedFinancialMapping
      ),
      Option.when(relevantErrors)(InvestmentReasons.InvalidProviderObservations),
      Option.when(snapshot.observedAt.isAfter(asOf))(InvestmentReasons.FinancialSnapshotFromFuture),
      Option.when(snapshot.observedAt.isBefore(asOf.minus(recipe.maxSnapshotAgeDays.days)))(InvestmentReasons.StaleFinancialSnapshot)
    ).flatten

  private def history(
      snapshot: FinancialSnapshot,
      metric: String,
      count: Int,
      today: LocalDate,
      recipe: InvestmentRecipe
  ): Either[String, List[FinancialObservation]] =
    val points = snapshot.annual.filter(_.metric == metric).sortBy(_.period).reverse.take(count)
    val complete = points.size == count && points.forall(point => !point.period.isAfter(today)) &&
      points.sliding(2).forall {
        case List(newer, older) =>
          val days = ChronoUnit.DAYS.between(older.period, newer.period)
          days >= 300 && days <= 430
        case _ => true
      }
    if !complete then Left(InvestmentReasons.insufficientAnnualHistory(metric))
    else if points.exists(_.unit != "ratio") then Left(InvestmentReasons.unverifiedUnit(metric))
    else if ChronoUnit.DAYS.between(points.head.period, today) > recipe.maxAnnualAgeDays then
      Left(InvestmentReasons.staleFiscalPeriod(metric))
    else Right(points)

  private def growthMetric(snapshot: FinancialSnapshot, metric: String): Either[String, BigDecimal] =
    snapshot.metrics.get(metric).map(_ / 100).toRight(InvestmentReasons.missingMetric(metric))

  private def median(points: List[FinancialObservation]): BigDecimal =
    val values = points.map(_.value).sorted
    values(values.size / 2)

  private def currentCashFlow(snapshot: FinancialSnapshot, today: LocalDate, recipe: InvestmentRecipe): Either[String, Unit] =
    snapshot.quarterly.filter(_.metric == "fcfPerShareTTM").sortBy(_.period).lastOption match
      case Some(point) if point.unit != "reporting-currency-per-share" => Left(InvestmentReasons.unverifiedUnit("fcfPerShareTTM"))
      case Some(point) if point.value > 0 && !point.period.isAfter(today) && ChronoUnit.DAYS.between(point.period, today) <= recipe.maxQuarterlyAgeDays =>
        Right(())
      case Some(_) => Left(InvestmentReasons.InvalidOrStaleCurrentFreeCashFlow)
      case None => Left(InvestmentReasons.MissingCurrentFreeCashFlow)
