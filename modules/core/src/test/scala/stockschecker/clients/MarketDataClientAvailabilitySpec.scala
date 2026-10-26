package stockschecker.clients

import cats.effect.{IO, Ref}
import org.mockito.Mockito.{mock, when}
import stockschecker.clients.finnhub.FinnhubClient
import stockschecker.clients.twelvedata.TwelveDataClient
import stockschecker.domain.PriceHistorySnapshot
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import stockschecker.repositories.{DataQualityRepository, ProviderBudgetRepository, RepositorySpec, SecurityRepository}

import java.time.{Instant, LocalDate}
import scala.concurrent.duration.*

class MarketDataClientAvailabilitySpec extends RepositorySpec {
  override def port: Int = 12158
  private val from       = LocalDate.parse("2025-01-01")
  private val through    = LocalDate.parse("2025-12-01")
  private val snapshot   = PriceHistorySnapshot(
    "test",
    AAPL,
    AAPLPriceCandles.toList,
    "TwelveData",
    "USD",
    "split-and-dividend",
    Instant.parse("2025-12-01T12:00:00Z"),
    from,
    through
  )
  private val limits = ProviderLimits(60000, 0, 1)

  "MarketDataClient availability" should {
    "recover from a provider 404 while preserving manual exclusions and successful-data timestamps" in
      withEmbeddedMongoDatabase { db =>
        val finnhub = mock(classOf[FinnhubClient[IO]])
        val twelve  = mock(classOf[TwelveDataClient[IO]])
        for {
          quality        <- DataQualityRepository.make[IO](db)
          budgets        <- ProviderBudgetRepository.make[IO](db)
          securities     <- SecurityRepository.make[IO](db)
          _              <- securities.save(List(AAPLSecurity, MSFTSecurity))
          _              <- securities.deactivate(AAPL)
          appleCalls     <- Ref.of[IO, Int](0)
          microsoftCalls <- Ref.of[IO, Int](0)
          _              <- IO {
            when(twelve.getDailyPriceHistory(AAPL, from, through, "all")).thenReturn(appleCalls.getAndUpdate(_ + 1).flatMap {
              case 0 => IO.raiseError(AppError.HttpClient("TwelveData", 404, "no data"))
              case 1 => IO.pure(snapshot)
              case _ => IO.raiseError(AppError.HttpClient("TwelveData", 503, "transient failure"))
            })
            when(twelve.getDailyPriceHistory(MSFT, from, through, "all")).thenReturn(microsoftCalls.getAndUpdate(_ + 1).flatMap {
              case 0 => IO.raiseError(AppError.HttpClient("TwelveData", 404, "no data"))
              case _ => IO.pure(snapshot.copy(ticker = MSFT))
            })
          }
          client      <- MarketDataClient.make[IO](finnhub, twelve, quality, limits, limits, budgets)
          first       <- client.getDailyPriceHistory(AAPL, from, through, "all").attempt
          unavailable <- quality.all
          _           <- IO.sleep(5.millis)
          _           <- client.getDailyPriceHistory(MSFT, from, through, "all").attempt
          _           <- IO.sleep(5.millis)
          recovered   <- client.getDailyPriceHistory(AAPL, from, through, "all")
          _           <- IO.sleep(5.millis)
          _           <- client.getDailyPriceHistory(MSFT, from, through, "all")
          available   <- quality.all
          _           <- IO.sleep(5.millis)
          _           <- client.getDailyPriceHistory(AAPL, from, through, "all").attempt
          failed      <- quality.all
          excluded    <- securities.find(AAPL)
          included    <- securities.find(MSFT)
        } yield {
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
      }

    "leave the last available or failed dataset result unchanged during provider deferrals" in
      withEmbeddedMongoDatabase { db =>
        val finnhub = mock(classOf[FinnhubClient[IO]])
        val twelve  = mock(classOf[TwelveDataClient[IO]])
        for
          quality <- DataQualityRepository.make[IO](db)
          budgets <- ProviderBudgetRepository.make[IO](db)
          now     <- IO.realTimeInstant
          _       <- quality.record(AAPL, "TwelveData", "daily-prices-all", "available", now, None)
          _       <- quality.record(MSFT, "TwelveData", "daily-prices-all", "failed", now, Some("HTTP 503"))
          before  <- quality.all
          _ = when(twelve.getDailyPriceHistory(AAPL, from, through, "all"))
            .thenReturn(IO.raiseError(AppError.ProviderDeferred("TwelveData", now.plusSeconds(300), "Retry-After")))
          client <- MarketDataClient.make[IO](finnhub, twelve, quality, limits, limits, budgets)
          first  <- client.getDailyPriceHistory(AAPL, from, through, "all").attempt
          second <- client.getDailyPriceHistory(MSFT, from, through, "all").attempt
          after  <- quality.all
        yield
          first.left.toOption.get mustBe a[AppError.ProviderDeferred]
          second.left.toOption.get mustBe a[AppError.ProviderDeferred]
          after.toSet mustBe before.toSet
      }

    "resume a disabled provider through the operational facade" in
      withEmbeddedMongoDatabase { db =>
        val finnhub = mock(classOf[FinnhubClient[IO]])
        val twelve  = mock(classOf[TwelveDataClient[IO]])
        for
          quality <- DataQualityRepository.make[IO](db)
          budgets <- ProviderBudgetRepository.make[IO](db)
          _ = when(twelve.getDailyPriceHistory(AAPL, from, through, "all"))
            .thenReturn(IO.raiseError(AppError.ProviderConfiguration("TwelveData", "expired credentials")), IO.pure(snapshot))
          client  <- MarketDataClient.make[IO](finnhub, twelve, quality, limits, limits, budgets)
          _       <- client.getDailyPriceHistory(AAPL, from, through, "all").attempt
          blocked <- client.getDailyPriceHistory(AAPL, from, through, "all").attempt
          _       <- client.resumeProvider("twelvedata")
          _       <- IO.sleep(5.millis)
          resumed <- client.getDailyPriceHistory(AAPL, from, through, "all")
          invalid <- client.resumeProvider("missing").attempt
        yield
          blocked.left.toOption.get mustBe a[AppError.ProviderConfiguration]
          resumed mustBe snapshot
          invalid.left.toOption.get mustBe a[AppError.FailedValidation]
      }
  }
}
