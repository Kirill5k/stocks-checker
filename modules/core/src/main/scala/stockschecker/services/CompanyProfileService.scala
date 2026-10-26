package stockschecker.services

import cats.effect.Sync
import cats.syntax.flatMap.*
import stockschecker.clients.MarketDataClient
import stockschecker.domain.errors.AppError
import stockschecker.domain.{CompanyProfile, CompanyProfileFilter, Ticker}
import stockschecker.repositories.{CompanyProfileRepository, SecurityRepository}

trait CompanyProfileService[F[_]]:
  def get(ticker: Ticker, fetchLatest: Boolean = false): F[CompanyProfile]
  def getAll(limit: Option[Int]): F[List[CompanyProfile]]
  def findTickersBy(filter: CompanyProfileFilter, limit: Option[Int] = None): F[List[Ticker]]
  def deactivate(ticker: Ticker): F[Unit]

final private class LiveCompanyProfileService[F[_]](
    private val repository: CompanyProfileRepository[F],
    private val client: MarketDataClient[F],
    private val securities: SecurityRepository[F]
)(using
    F: Sync[F]
) extends CompanyProfileService[F] {

  override def get(ticker: Ticker, fetch: Boolean = false): F[CompanyProfile] = {
    val cpOpt =
      if (fetch) fetchCompanyProfile(ticker).flatTap(cp => F.whenA(cp.nonEmpty)(save(cp.toList)))
      else repository.find(ticker)

    cpOpt.flatMap(cp => F.fromOption(cp, AppError.CompanyProfileNotFound(ticker)))
  }

  override def getAll(limit: Option[Int]): F[List[CompanyProfile]] =
    repository.findAll(limit)

  private def fetchCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]] =
    client.getCompanyProfile(ticker)

  override def findTickersBy(filter: CompanyProfileFilter, limit: Option[Int] = None): F[List[Ticker]] =
    repository.findTickersBy(filter, limit)

  private def save(cps: List[CompanyProfile]): F[Unit] =
    repository.save(cps) >> securities.updateCompanyProfileLastUpdated(cps.map(_.ticker))

  override def deactivate(ticker: Ticker): F[Unit] =
    repository.deactivate(ticker)
}

object CompanyProfileService:
  def make[F[_]: Sync](
      repo: CompanyProfileRepository[F],
      client: MarketDataClient[F],
      securities: SecurityRepository[F]
  ): F[CompanyProfileService[F]] =
    Sync[F].pure(LiveCompanyProfileService[F](repo, client, securities))
