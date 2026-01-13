package stockschecker.actions

import cats.data.NonEmptyList
import stockschecker.common.JsonCodecs
import stockschecker.domain.{CommandId, CompanyProfileFilter, Exchange, SecurityFilter, Ticker}
import org.latestbit.circe.adt.codec.*

import scala.concurrent.duration.FiniteDuration

enum Action derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case RescheduleAll
  case Schedule(cid: CommandId, duration: FiniteDuration)
  case Sequence(actions: NonEmptyList[Action])
  // Fetch operations - initial data retrieval
  case FetchSecurities(exchanges: NonEmptyList[Exchange])
  case FetchCompanyProfiles(filter: SecurityFilter, limit: Option[Int] = None)
  case FetchFinancialMetrics(filter: CompanyProfileFilter, limit: Option[Int] = None)
  case FetchPriceAnalytics(filter: CompanyProfileFilter, limit: Option[Int] = None)
  // Update operations - processing/analysis
  case UpdateCompanyProfiles(tickers: NonEmptyList[Ticker])
  case UpdateFinancialMetrics(tickers: NonEmptyList[Ticker])
  case UpdatePriceAnalytics(tickers: NonEmptyList[Ticker])
  // Record operations - audit/tracking
  case RecordCompanyProfileUpdate(tickers: List[Ticker])
  case RecordFinancialMetricsUpdate(tickers: List[Ticker])
  case RecordPriceAnalyticsUpdate(tickers: List[Ticker])

object Action extends JsonCodecs {
  given JsonTaggedAdt.Config[Action] = JsonTaggedAdt.Config.Values[Action](
    mappings = Map(
      "reschedule-all"                  -> JsonTaggedAdt.tagged[Action.RescheduleAll.type],
      "schedule"                        -> JsonTaggedAdt.tagged[Action.Schedule],
      "sequence"                        -> JsonTaggedAdt.tagged[Action.Sequence],
      "fetch-securities"                -> JsonTaggedAdt.tagged[Action.FetchSecurities],
      "fetch-company-profiles"          -> JsonTaggedAdt.tagged[Action.FetchCompanyProfiles],
      "fetch-financial-metrics"         -> JsonTaggedAdt.tagged[Action.FetchFinancialMetrics],
      "fetch-price-analytics"           -> JsonTaggedAdt.tagged[Action.FetchPriceAnalytics],
      "update-company-profiles"         -> JsonTaggedAdt.tagged[Action.UpdateCompanyProfiles],
      "update-financial-metrics"        -> JsonTaggedAdt.tagged[Action.UpdateFinancialMetrics],
      "update-price-analytics"          -> JsonTaggedAdt.tagged[Action.UpdatePriceAnalytics],
      "record-company-profile-update"   -> JsonTaggedAdt.tagged[Action.RecordCompanyProfileUpdate],
      "record-financial-metrics-update" -> JsonTaggedAdt.tagged[Action.RecordFinancialMetricsUpdate],
      "record-price-analytics-update"   -> JsonTaggedAdt.tagged[Action.RecordPriceAnalyticsUpdate]
    ),
    strict = true,
    typeFieldName = "kind"
  )
}
