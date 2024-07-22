package stockschecker.services

import cats.MonadThrow
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import stockschecker.clients.MarketDataClient
import stockschecker.domain.errors.AppError
import stockschecker.domain.{CompanyProfile, Ticker}
import stockschecker.repositories.CompanyProfileRepository

trait CompanyProfileService[F[_]]:
  def get(ticker: Ticker): F[CompanyProfile]

final private class LiveCompanyProfileService[F[_]](
    private val repository: CompanyProfileRepository[F],
    private val client: MarketDataClient[F]
)(using
    F: MonadThrow[F]
) extends CompanyProfileService[F] {

  private def unfoldOpt[A](ifPresent: A => F[A], ifMissing: => F[A])(opt: Option[A]): F[A] =
    opt match
      case Some(value) => ifPresent(value)
      case None        => ifMissing

  override def get(ticker: Ticker): F[CompanyProfile] =
    repository
      .find(ticker)
      .flatMap(
        unfoldOpt(
          cp => F.pure(cp),
          client
            .getCompanyProfile(ticker)
            .flatMap(
              unfoldOpt(
                cp => repository.save(cp).as(cp),
                F.raiseError(AppError.NotFound(s"Couldn't not find company profile for $ticker"))
              )
            )
        )
      )
}

object CompanyProfileService:
  def make[F[_]](repo: CompanyProfileRepository[F], client: MarketDataClient[F])(using F: MonadThrow[F]): F[CompanyProfileService[F]] =
    F.pure(LiveCompanyProfileService[F](repo, client))