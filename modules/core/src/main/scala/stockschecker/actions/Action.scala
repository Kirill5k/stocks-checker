package stockschecker.actions

import cats.data.NonEmptyList
import io.circe.Codec
import io.circe.derivation.{Configuration, ConfiguredCodec}
import stockschecker.common.JsonCodecs
import stockschecker.domain.{CompanyProfileFilter, Exchange, SecurityFilter, Ticker}

import java.time.LocalDate

enum Action:
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
  case RefreshInvestmentResearch
  case RefreshEvaluationPrices(ticker: Ticker, from: LocalDate, through: LocalDate)
  // Deactivate operations
  case DeactivateSecurity(ticker: Ticker)
  case DeactivateCompanyProfile(ticker: Ticker)

object Action extends JsonCodecs {
  given Configuration          = Configuration.default.withKebabCaseConstructorNames.withDiscriminator("kind").withDefaults
  given Codec.AsObject[Action] = ConfiguredCodec.derived[Action]
}
