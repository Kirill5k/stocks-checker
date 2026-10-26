package stockschecker.actions

import cats.data.NonEmptyList
import io.circe.derivation.{Configuration, ConfiguredCodec}
import stockschecker.common.JsonCodecs
import stockschecker.domain.{CommandId, CompanyProfileFilter, Exchange, SecurityFilter, Ticker}

import java.time.LocalDate
import scala.concurrent.duration.FiniteDuration

enum Action derives ConfiguredCodec:
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
  case RefreshEvaluationPrices(ticker: Ticker, from: LocalDate, through: LocalDate)
  // Record operations - audit/tracking
  case RecordCompanyProfileUpdate(tickers: List[Ticker])
  case RecordFinancialMetricsUpdate(tickers: List[Ticker])
  case RecordPriceAnalyticsUpdate(tickers: List[Ticker])
  // Deactivate operations
  case DeactivateSecurity(ticker: Ticker)
  case DeactivateCompanyProfile(ticker: Ticker)
  // Retry wrapper
  case Retried(action: Action, attempt: Int)

object Action extends JsonCodecs {
  given Configuration = Configuration.default.withKebabCaseConstructorNames.withDiscriminator("kind").withDefaults
}
