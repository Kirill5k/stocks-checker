package stockschecker.clients

import cats.effect.{IO, Ref}
import stockschecker.domain.{PriceAdjustment, PriceHistorySnapshot, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import stockschecker.repositories.{DataQualityRepository, ProviderBudgetRepository, RepositorySpec, SecurityRepository}

import java.time.{Instant, LocalDate}
import scala.concurrent.duration.*

class ProviderAvailabilitySpec extends RepositorySpec {
  override def port: Int = 12158
  private val from       = LocalDate.parse("2025-01-01")
  private val through    = LocalDate.parse("2025-12-01")
  private val snapshot   = PriceHistorySnapshot(
    "test",
    AAPL,
    AAPLPriceCandles.toList,
    "TwelveData",
    "USD",
    PriceAdjustment.SplitsAndDividends,
    Instant.parse("2025-12-01T12:00:00Z"),
    from,
    through
  )
  private val limits = ProviderLimits(60000, 0, 1)

  private def observed(access: ProviderAccess[IO], ticker: Ticker)(request: IO[PriceHistorySnapshot]): IO[PriceHistorySnapshot] =
    access.observed(ticker, "TwelveData", "daily-prices-all")(access.run("TwelveData", limits)(request))(_.candles.nonEmpty)

  "Provider availability" should {
    "recover from a provider 404 while preserving manual exclusions and successful-data timestamps" in
      withEmbeddedMongoDatabase { db =>
        for
          quality        <- DataQualityRepository.make[IO](db)
          budgets        <- ProviderBudgetRepository.make[IO](db)
          access         <- ProviderAccess.make[IO](budgets, quality)
          securities     <- SecurityRepository.make[IO](db)
          _              <- securities.save(List(AAPLSecurity, MSFTSecurity))
          _              <- securities.deactivate(AAPL)
          appleCalls     <- Ref.of[IO, Int](0)
          microsoftCalls <- Ref.of[IO, Int](0)
          apple = observed(access, AAPL)(appleCalls.getAndUpdate(_ + 1).flatMap {
            case 0 => IO.raiseError(AppError.HttpClient("TwelveData", 404, "no data"))
            case 1 => IO.pure(snapshot)
            case _ => IO.raiseError(AppError.HttpClient("TwelveData", 503, "transient failure"))
          })
          microsoft = observed(access, MSFT)(microsoftCalls.getAndUpdate(_ + 1).flatMap {
            case 0 => IO.raiseError(AppError.HttpClient("TwelveData", 404, "no data"))
            case _ => IO.pure(snapshot.copy(ticker = MSFT))
          })
          first       <- apple.attempt
          unavailable <- quality.all
          _           <- IO.sleep(5.millis)
          _           <- microsoft.attempt
          _           <- IO.sleep(5.millis)
          recovered   <- apple
          _           <- IO.sleep(5.millis)
          _           <- microsoft
          available   <- quality.all
          _           <- IO.sleep(5.millis)
          _           <- apple.attempt
          failed      <- quality.all
          excluded    <- securities.find(AAPL)
          included    <- securities.find(MSFT)
        yield
          first.left.toOption.collect { case e: AppError.HttpClient => e.status } mustBe Some(404)
          unavailable.find(_.ticker == AAPL).map(_.status) mustBe Some("unavailable")
          unavailable.find(_.ticker == AAPL).flatMap(_.lastSuccessAt) mustBe None
          recovered mustBe snapshot
          available.map(_.status).toSet mustBe Set("available")
          available.flatMap(_.lastSuccessAt).size mustBe 2
          val successfulApple = available.find(_.ticker == AAPL).get
          val failedApple     = failed.find(_.ticker == AAPL).get
          failedApple.status mustBe "failed"
          failedApple.lastSuccessAt mustBe successfulApple.lastSuccessAt
          failedApple.failure mustBe Some("HTTP 503")
          excluded.map(_.isActive) mustBe Some(false)
          excluded.flatMap(_.exclusionReason) mustBe Some("user-excluded")
          included.map(_.isActive) mustBe Some(true)
          included.flatMap(_.exclusionReason) mustBe None
      }

    "leave the last available or failed dataset result unchanged during provider and budget deferrals" in
      withEmbeddedMongoDatabase { db =>
        for
          quality <- DataQualityRepository.make[IO](db)
          budgets <- ProviderBudgetRepository.make[IO](db)
          access  <- ProviderAccess.make[IO](budgets, quality)
          now     <- IO.realTimeInstant
          _       <- quality.record(AAPL, "TwelveData", "daily-prices-all", "available", now, None)
          _       <- quality.record(MSFT, "TwelveData", "daily-prices-all", "failed", now, Some("HTTP 503"))
          before  <- quality.all
          first <- observed(access, AAPL)(
            IO.raiseError(AppError.ProviderDeferred("TwelveData", now.plusSeconds(300), "Retry-After"))
          ).attempt
          second <- observed(access, MSFT)(IO.raiseError(new AssertionError("Deferred request ran"))).attempt
          after  <- quality.all
        yield
          first.left.toOption.get mustBe a[AppError.ProviderDeferred]
          second.left.toOption.get mustBe a[AppError.ProviderDeferred]
          after.toSet mustBe before.toSet
      }
  }
}
