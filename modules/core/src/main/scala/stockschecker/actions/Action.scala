package stockschecker.actions

import cats.data.NonEmptyList
import stockschecker.common.JsonCodecs
import stockschecker.domain.{CommandId, CompanyProfileFilter, Exchange, SecurityFilter, Ticker}
import org.latestbit.circe.adt.codec.*

import scala.concurrent.duration.FiniteDuration

enum Action derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case RescheduleAll
  case Schedule(cid: CommandId, duration: FiniteDuration)
  case DiscoverSecurities(exchanges: NonEmptyList[Exchange])
  case EnrichCompanyProfiles(filter: SecurityFilter, limit: Option[Int] = None)
  case FetchPricePerformanceSummaries(filter: CompanyProfileFilter, limit: Option[Int] = None)
  case FetchCompanyProfiles(ticker: NonEmptyList[Ticker])
  case UpdatePriceAnalysis(tickers: NonEmptyList[Ticker])
  case RecordPriceAnalyticsUpdate(tickers: List[Ticker])
  case RecordCompanyProfileUpdate(tickers: List[Ticker])
  case Sequence(actions: NonEmptyList[Action])

object Action extends JsonCodecs {
  given JsonTaggedAdt.Config[Action] = JsonTaggedAdt.Config.Values[Action](
    mappings = Map(
      "discover-securities"                  -> JsonTaggedAdt.tagged[Action.DiscoverSecurities],
      "enrich-company-profiles"              -> JsonTaggedAdt.tagged[Action.EnrichCompanyProfiles],
      "fetch-price-performance-summaries"    -> JsonTaggedAdt.tagged[Action.FetchPricePerformanceSummaries],
      "fetch-company-profiles"               -> JsonTaggedAdt.tagged[Action.FetchCompanyProfiles],
      "update-price-analysis"                -> JsonTaggedAdt.tagged[Action.UpdatePriceAnalysis],
      "record-company-profile-update"        -> JsonTaggedAdt.tagged[Action.RecordCompanyProfileUpdate],
      "record-price-analytics-update"        -> JsonTaggedAdt.tagged[Action.RecordPriceAnalyticsUpdate],
      "schedule"                             -> JsonTaggedAdt.tagged[Action.Schedule],
      "reschedule-all"                       -> JsonTaggedAdt.tagged[Action.RescheduleAll.type],
      "sequence"                             -> JsonTaggedAdt.tagged[Action.Sequence]
    ),
    strict = true,
    typeFieldName = "kind"
  )
}
