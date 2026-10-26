package stockschecker.services

import java.time.LocalDate
import stockschecker.domain.{EvaluationPriceSeries, Ticker}

/** A daily, total-return history. Providers must disclose incomplete or unaudited coverage. */
trait EvaluationPriceHistory[F[_]]:
  def history(ticker: Ticker, from: LocalDate, through: LocalDate): F[EvaluationPriceSeries]
  def cached(ticker: Ticker, from: LocalDate, through: LocalDate): F[Option[EvaluationPriceSeries]]
