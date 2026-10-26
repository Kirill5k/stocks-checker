package stockschecker.clients

import cats.effect.{IO, Ref}
import org.mockito.Mockito.{mock, when}
import stockschecker.clients.finnhub.FinnhubClient
import stockschecker.clients.twelvedata.TwelveDataClient
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import stockschecker.repositories.{DataQualityRepository, RepositorySpec, SecurityRepository}

import scala.concurrent.duration.*

class MarketDataClientAvailabilitySpec extends RepositorySpec {
  override def port: Int = 12158

  "MarketDataClient availability" should {
    "recover from a provider 404 while preserving manual exclusions and successful-data timestamps" in
      withEmbeddedMongoDatabase { db =>
        val finnhub = mock(classOf[FinnhubClient[IO]])
        val twelve = mock(classOf[TwelveDataClient[IO]])
        for {
          quality <- DataQualityRepository.make[IO](db)
          securities <- SecurityRepository.make[IO](db)
          _ <- securities.save(List(AAPLSecurity, MSFTSecurity))
          _ <- securities.deactivate(AAPL)
          appleCalls <- Ref.of[IO, Int](0)
          microsoftCalls <- Ref.of[IO, Int](0)
          _ <- IO {
            when(twelve.getMonthlyPriceCandles(AAPL)).thenReturn(appleCalls.getAndUpdate(_ + 1).flatMap {
              case 0 => IO.raiseError(AppError.HttpClient("TwelveData", 404, "no data"))
              case 1 => IO.pure(AAPLPriceCandles)
              case _ => IO.raiseError(AppError.HttpClient("TwelveData", 503, "transient failure"))
            })
            when(twelve.getMonthlyPriceCandles(MSFT)).thenReturn(microsoftCalls.getAndUpdate(_ + 1).flatMap {
              case 0 => IO.raiseError(AppError.HttpClient("TwelveData", 404, "no data"))
              case _ => IO.pure(AAPLPriceCandles)
            })
          }
          client <- MarketDataClient.make[IO](finnhub, twelve, Some(quality), twelveLimits = ProviderLimits(60000, 0, 1))
          first <- client.getMonthlyPriceCandles(AAPL).attempt
          unavailable <- quality.all
          _ <- IO.sleep(5.millis)
          _ <- client.getMonthlyPriceCandles(MSFT).attempt
          _ <- IO.sleep(5.millis)
          recovered <- client.getMonthlyPriceCandles(AAPL)
          _ <- IO.sleep(5.millis)
          _ <- client.getMonthlyPriceCandles(MSFT)
          available <- quality.all
          _ <- IO.sleep(5.millis)
          _ <- client.getMonthlyPriceCandles(AAPL).attempt
          failed <- quality.all
          excluded <- securities.find(AAPL)
          included <- securities.find(MSFT)
        } yield {
          first.left.toOption.collect { case e: AppError.HttpClient => e.status } mustBe Some(404)
          unavailable.find(_.ticker == AAPL).map(_.status) mustBe Some("unavailable")
          unavailable.find(_.ticker == AAPL).flatMap(_.lastSuccessAt) mustBe None
          recovered mustBe AAPLPriceCandles
          available.map(_.status).toSet mustBe Set("available")
          available.flatMap(_.lastSuccessAt).size mustBe 2
          val successfulApple = available.find(_.ticker == AAPL).get
          val failedApple = failed.find(_.ticker == AAPL).get
          failedApple.status mustBe "failed"
          failedApple.lastSuccessAt mustBe successfulApple.lastSuccessAt
          failedApple.failure mustBe Some("HTTP 503")
          excluded.map(_.isActive) mustBe Some(false)
          excluded.flatMap(_.exclusionReason) mustBe Some("user-excluded")
          included.map(_.isActive) mustBe Some(true)
          included.flatMap(_.exclusionReason) mustBe None
        }
      }
  }
}
