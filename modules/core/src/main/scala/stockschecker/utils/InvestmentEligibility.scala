package stockschecker.utils

import stockschecker.domain.*

/** Financial rules shared by ranking and explanations of excluded candidates. */
object InvestmentEligibility:
  val CashFlowHistoryYears           = 5
  val MinimumPositiveCashFlowYears   = 4
  val InsufficientCashFlowDurability = "insufficient-cash-flow-durability"

  def positiveMetric(metric: String, metrics: Map[String, BigDecimal]): Either[String, BigDecimal] =
    metrics.get(metric) match
      case Some(value) if value > 0 => Right(value)
      case Some(_)                  => Left(s"nonpositive-metric:$metric")
      case None                     => Left(s"missing-metric:$metric")

  def hasDurableCashFlow(margins: List[FinancialObservation]): Boolean =
    margins.headOption.exists(_.value > 0) && margins.count(_.value > 0) >= MinimumPositiveCashFlowYears

  def explainExclusion(card: InvestmentScorecard, snapshot: Option[FinancialSnapshot]): Option[String] =
    card.exclusions match
      case InsufficientCashFlowDurability :: Nil =>
        card.evidence.map(_.financialObservations).orElse(snapshot.map(_.annual)).flatMap { observations =>
          val margins = observations.filter(_.metric == "fcfMargin").sortBy(_.period).reverse.take(CashFlowHistoryYears)
          Option.when(margins.size == CashFlowHistoryYears)(
            s"cash-flow durability: latest annual margin ${margins.head.value}; ${margins.count(_.value > 0)}/$CashFlowHistoryYears positive years; requires latest > 0 and at least $MinimumPositiveCashFlowYears/$CashFlowHistoryYears positive years"
          )
        }
      case reason :: Nil if Set("nonpositive-metric:peTTM", "nonpositive-metric:pfcfShareTTM", "nonpositive-metric:epsTTM")(reason) =>
        val metric = reason.stripPrefix("nonpositive-metric:")
        snapshot.flatMap(_.metrics.get(metric)).map(value => s"$metric = $value; requires a positive value")
      case _ => None
