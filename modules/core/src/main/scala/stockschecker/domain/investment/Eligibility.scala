package stockschecker.domain.investment

import stockschecker.domain.*

import java.time.{Instant, ZoneOffset}
import java.time.temporal.ChronoUnit

/** Financial rules shared by ranking and explanations of excluded candidates. */
object Eligibility:
  val CashFlowHistoryYears           = 5
  val MinimumPositiveCashFlowYears   = 4
  val InsufficientCashFlowDurability = InvestmentReasons.InsufficientCashFlowDurability

  def positiveMetric(metric: FinancialMetric, metrics: Map[FinancialMetric, BigDecimal]): Either[String, BigDecimal] =
    metrics.get(metric) match
      case Some(value) if value > 0 => Right(value)
      case Some(_)                  => Left(InvestmentReasons.nonpositiveMetric(metric.print))
      case None                     => Left(InvestmentReasons.missingMetric(metric.print))

  def hasDurableCashFlow(margins: List[FinancialObservation]): Boolean =
    margins.headOption.exists(_.value > 0) && margins.count(_.value > 0) >= MinimumPositiveCashFlowYears

  def explainExclusion(card: InvestmentScorecard, snapshot: Option[FinancialSnapshot]): Option[String] =
    card.exclusions match
      case InsufficientCashFlowDurability :: Nil =>
        card.evidence.map(_.financialObservations).orElse(snapshot.map(_.annual)).flatMap { observations =>
          val margins =
            observations.filter(_.metric == FinancialMetric.FreeCashFlowMargin).sortBy(_.period).reverse.take(CashFlowHistoryYears)
          Option.when(margins.size == CashFlowHistoryYears)(
            s"cash-flow durability: latest annual margin ${margins.head.value}; ${margins.count(_.value > 0)}/$CashFlowHistoryYears positive years; requires latest > 0 and at least $MinimumPositiveCashFlowYears/$CashFlowHistoryYears positive years"
          )
        }
      case reason :: Nil =>
        List(FinancialMetric.PriceToEarningsTtm, FinancialMetric.PriceToFreeCashFlowTtm, FinancialMetric.EpsTtm)
          .find(metric => reason == InvestmentReasons.nonpositiveMetric(metric.print))
          .flatMap(metric => snapshot.flatMap(_.metrics.get(metric)).map(value => s"${metric.print} = $value; requires a positive value"))
      case _ => None

  def stockReasons(stock: Stock, asOf: Instant, recipe: InvestmentRecipe): List[String] =
    val sector     = stock.profile.flatMap(_.sector)
    val today      = asOf.atZone(ZoneOffset.UTC).toLocalDate
    val validPrice = stock.priceAnalytics.exists { analytics =>
      val p = analytics.performanceSummary
      p.latestPrice > 0 && !p.latestPriceDate.isAfter(today) && ChronoUnit.DAYS.between(p.latestPriceDate, today) <= recipe.maxPriceAgeDays
    }
    List(
      Option.when(!stock.security.isActive)(InvestmentReasons.InactiveSecurity),
      Option.when(stock.security.listingStatus != ListingStatus.Listed)(InvestmentReasons.SecurityNotListed),
      stock.security.exclusionReason.map(reason => InvestmentReasons.securityExcluded(reason)),
      Option.when(!validPrice)(InvestmentReasons.MissingInvalidOrStalePrice),
      Option.when(stock.security.kind != SecurityKind.Stock)(InvestmentReasons.SpecialistSecurityModelRequired),
      Option.when(!stock.profile.exists(_.country.equalsIgnoreCase("US")))(InvestmentReasons.OutsideUsCompanyUniverse),
      Option.when(!stock.profile.exists(_.currency.equalsIgnoreCase("USD")))(InvestmentReasons.UnsupportedReportingCurrency),
      Option.when(sector.isEmpty)(InvestmentReasons.UnmappedIndustry),
      Option.when(sector.exists(Sector.requiresSpecialistModel))(InvestmentReasons.SpecialistSectorModelRequired)
    ).flatten

  def priceWarnings(stock: Stock): List[String] =
    Option
      .when(!stock.priceAnalytics.exists(pa => pa.metrics.cagr5Year.nonEmpty && pa.metrics.volatility.nonEmpty))(
        InvestmentReasons.InsufficientComparablePriceRiskHistory
      )
      .toList
