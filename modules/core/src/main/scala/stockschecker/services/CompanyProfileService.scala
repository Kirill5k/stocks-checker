package stockschecker.services

import cats.MonadThrow
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import fs2.Stream
import stockschecker.clients.MarketDataClient
import stockschecker.domain.errors.AppError
import stockschecker.domain.{CompanyProfile, CompanyProfileFilter, Ticker}
import stockschecker.repositories.CompanyProfileRepository

trait CompanyProfileService[F[_]]:
  def save(cps: List[CompanyProfile]): F[Unit]
  def get(ticker: Ticker, fetchLatest: Boolean = false): F[CompanyProfile]
  def getAll(limit: Option[Int]): F[List[CompanyProfile]]
  def fetchLatest(ticker: Ticker, save: Boolean = true): F[Option[CompanyProfile]]
  def streamTickersBy(filter: CompanyProfileFilter, limit: Option[Int] = None): Stream[F, Ticker]

final private class LiveCompanyProfileService[F[_]](
    private val repository: CompanyProfileRepository[F],
    private val client: MarketDataClient[F]
)(using
    F: MonadThrow[F]
) extends CompanyProfileService[F] {

  override def fetchLatest(ticker: Ticker, save: Boolean = true): F[Option[CompanyProfile]] =
    fetchCompanyProfile(ticker)
      .flatMap {
        case Some(cp)     => F.whenA(save)(repository.save(cp)).as(Some(cp))
        case None if save => F.raiseError(AppError.CompanyProfileNotFound(ticker))
        case None         => F.pure(None)
      }

  override def get(ticker: Ticker, fetch: Boolean = false): F[CompanyProfile] = {
    val cpOpt =
      if (fetch) fetchLatest(ticker)
      else repository.find(ticker)
      
    cpOpt.flatMap(cp => F.fromOption(cp, AppError.CompanyProfileNotFound(ticker)))
  }

  override def getAll(limit: Option[Int]): F[List[CompanyProfile]] =
    repository.findAll(limit)

  private def fetchCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]] =
    client.getCompanyProfile(ticker)

  override def streamTickersBy(filter: CompanyProfileFilter, limit: Option[Int] = None): Stream[F, Ticker] =
    repository.streamTickersBy(filter, limit)

  override def save(cps: List[CompanyProfile]): F[Unit] =
    repository.save(cps)
}

object CompanyProfileService:
  def make[F[_]](repo: CompanyProfileRepository[F], client: MarketDataClient[F])(using F: MonadThrow[F]): F[CompanyProfileService[F]] =
    F.pure(LiveCompanyProfileService[F](repo, client))
