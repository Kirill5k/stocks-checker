package stockschecker.clients.finnhub

import kirill5k.common.cats.Clock
import cats.effect.{Deferred, IO}
import kirill5k.common.sttp.test.Sttp4WordSpec
import stockschecker.clients.{DatasetAvailabilityObserver, ProviderAccess, ProviderLimits}
import stockschecker.common.config.FinnhubClientConfig
import stockschecker.domain.{CompanyProfile, Exchange, FinancialMetrics, Security, SecurityKind, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.{DataQualityRepository, ProviderBudgetRepository}
import sttp.client4.testing.ResponseStub
import fs2.Stream
import sttp.model.StatusCode

import java.time.{Instant, LocalDate}

class FinnhubClientSpec extends Sttp4WordSpec {
  given Clock[IO]                                   = Clock.make[IO]
  private def budgets: ProviderBudgetRepository[IO] = {
    val repository = mock[ProviderBudgetRepository[IO]]
    when(repository.reserve(anyString, any[Instant], any[Long], any[Int], any[Option[String]])).thenReturnIO(None)
    when(repository.cooldown(anyString, any[Instant], any[Option[String]])).thenReturnUnit
    repository
  }

  private def quality: DataQualityRepository[IO] = {
    val repository = mock[DataQualityRepository[IO]]
    when(repository.record(any[Ticker], anyString, anyString, anyString, any[Instant], any[Option[String]])).thenReturnUnit
    repository
  }

  private def providerAccess: IO[ProviderAccess[IO]] = ProviderAccess.make[IO](budgets)

  "A FinnhubClient" when {

    val config = FinnhubClientConfig("http://finnhub.io", "api-key", ProviderLimits(60, 0, 2))

    "getListedSecurities" should {
      "return list of securities for NASDAQ on success" in {
        val expectedParams = Map("token" -> "api-key", "exchange" -> "US", "mic" -> "XNAS")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/api/v1/stock/symbol") && r.hasParams(expectedParams) =>
              ResponseStub.adjust(Stream.emit(readJson("finnhub/list-stocks-success.json")).through(fs2.text.utf8.encode))
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          access     <- providerAccess
          observer   <- DatasetAvailabilityObserver.make[IO](quality)
          client     <- FinnhubClient.make[IO](config, testingBackend, access, observer)
          securities <- client.getListedSecurities(Exchange.NASDAQ).compile.toList
        yield securities

        result.asserting { s =>
          s must have size 2
          s.head mustBe Security(
            ticker = Ticker("RMSGW"),
            exchange = Exchange.NASDAQ,
            name = "REAL MESSENGER CORP-28",
            kind = SecurityKind.Other
          )
          s(1) mustBe Security(
            ticker = Ticker("FOXX"),
            exchange = Exchange.NASDAQ,
            name = "FOXX DEVELOPMENT HOLDINGS IN",
            kind = SecurityKind.Stock
          )
        }
      }

      "return list of securities for NYSE on success" in {
        val expectedParams = Map("token" -> "api-key", "exchange" -> "US", "mic" -> "XNYS")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/api/v1/stock/symbol") && r.hasParams(expectedParams) =>
              ResponseStub.adjust(Stream.emit(readJson("finnhub/list-stocks-success.json")).through(fs2.text.utf8.encode))
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          access     <- providerAccess
          observer   <- DatasetAvailabilityObserver.make[IO](quality)
          client     <- FinnhubClient.make[IO](config, testingBackend, access, observer)
          securities <- client.getListedSecurities(Exchange.NYSE).compile.toList
        yield securities

        result.asserting { s =>
          s must have size 2
        }
      }
    }

    "getCompanyProfile" should {
      "return company profile on success" in {
        val expectedParams = Map("token" -> "api-key", "symbol" -> "AAPL")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/api/v1/stock/profile2") && r.hasParams(expectedParams) =>
              ResponseStub.adjust(readJson("finnhub/company-profile-success.json"))
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          access   <- providerAccess
          observer <- DatasetAvailabilityObserver.make[IO](quality)
          client   <- FinnhubClient.make[IO](config, testingBackend, access, observer)
          profile  <- client.getCompanyProfile(Ticker("AAPL"))
        yield profile

        result.asserting { cp =>
          cp mustBe Some(
            CompanyProfile(
              ticker = Ticker("AAPL"),
              name = "APPLE INC",
              country = "US",
              industry = "Technology",
              description = Some(
                "Apple Inc. designs, manufactures, and markets smartphones, personal computers, tablets, wearables, and accessories worldwide."
              ),
              website = "https://www.apple.com/",
              ipoDate = Some(LocalDate.parse("1980-12-12")),
              currency = "USD",
              marketCap = 3989245252568L
            )
          )
        }
      }

      "return None when company profile is not found" in {
        val expectedParams = Map("token" -> "api-key", "symbol" -> "INVALID")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/api/v1/stock/profile2") && r.hasParams(expectedParams) =>
              ResponseStub.adjust(readJson("finnhub/company-profile-not-found.json"))
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          access   <- providerAccess
          observer <- DatasetAvailabilityObserver.make[IO](quality)
          client   <- FinnhubClient.make[IO](config, testingBackend, access, observer)
          profile  <- client.getCompanyProfile(Ticker("INVALID"))
        yield profile

        result.asserting { cp =>
          cp mustBe None
        }
      }

      "defer rate-limited work without an unbounded retry" in {
        val testingBackend = fs2BackendStub.whenAnyRequest
          .thenRespondCyclic(
            ResponseStub.adjust("""{"error":"API limit exceeded"}""", StatusCode.TooManyRequests),
            ResponseStub.adjust(readJson("finnhub/company-profile-success.json"))
          )

        val result = for
          access   <- providerAccess
          observer <- DatasetAvailabilityObserver.make[IO](quality)
          client   <- FinnhubClient.make[IO](config, testingBackend, access, observer)
          profile  <- client.getCompanyProfile(Ticker("AAPL"))
        yield profile

        result.attempt.asserting { response =>
          response.left.toOption.get mustBe a[AppError.ProviderDeferred]
        }
      }
    }

    "getFinancialData" should {
      "return financial metrics on success" in {
        val expectedParams = Map("token" -> "api-key", "symbol" -> "AAPL", "metric" -> "all")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/api/v1/stock/metric") && r.hasParams(expectedParams) =>
              ResponseStub.adjust(readJson("finnhub/basic-financials-success.json"))
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          access   <- providerAccess
          observer <- DatasetAvailabilityObserver.make[IO](quality)
          client   <- FinnhubClient.make[IO](config, testingBackend, access, observer)
          metrics  <- client.getFinancialData(Ticker("AAPL"))
        yield metrics

        result.asserting { m =>
          m.map(_.metrics) mustBe Some(
            FinancialMetrics(
              ticker = Ticker("AAPL"),
              peRatioTtm = Some(BigDecimal(34.1119)),
              epsTtm = Some(BigDecimal("7.459300000000001")),
              roeTtm = Some(BigDecimal(164.05)),
              dividendYieldAnnual = Some(BigDecimal(0.40148234)),
              debtToEquityAnnual = Some(BigDecimal(1.338)),
              profitMarginTtm = Some(BigDecimal(26.92)),
              freeCashFlowPerShareTtm = Some(BigDecimal(6.6855)),
              revenueGrowth5Y = Some(BigDecimal(8.68)),
              epsGrowth5Y = Some(BigDecimal(17.91)),
              priceHigh52Week = Some(BigDecimal(288.62)),
              priceLow52Week = Some(BigDecimal(169.2101))
            )
          )
        }
      }

      "return None when financial metrics are not found" in {
        val expectedParams = Map("token" -> "api-key", "symbol" -> "INVALID", "metric" -> "all")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/api/v1/stock/metric") && r.hasParams(expectedParams) =>
              ResponseStub.adjust(readJson("finnhub/basic-financials-not-found.json"))
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          access   <- providerAccess
          observer <- DatasetAvailabilityObserver.make[IO](quality)
          client   <- FinnhubClient.make[IO](config, testingBackend, access, observer)
          metrics  <- client.getFinancialData(Ticker("INVALID"))
        yield metrics

        result.asserting { m =>
          m mustBe None
        }
      }

      "defer rate-limited work without an unbounded retry" in {
        val testingBackend = fs2BackendStub.whenAnyRequest
          .thenRespondCyclic(
            ResponseStub.adjust("""{"error":"API limit exceeded"}""", StatusCode.TooManyRequests),
            ResponseStub.adjust(readJson("finnhub/basic-financials-success.json"))
          )

        val result = for
          access   <- providerAccess
          observer <- DatasetAvailabilityObserver.make[IO](quality)
          client   <- FinnhubClient.make[IO](config, testingBackend, access, observer)
          profile  <- client.getFinancialData(Ticker("AAPL"))
        yield profile

        result.attempt.asserting { response =>
          response.left.toOption.get mustBe a[AppError.ProviderDeferred]
        }
      }
    }

    "provider access" should {
      "reject a direct request when its shared budget is exhausted without changing data quality" in {
        val repository     = budgets
        val availability   = quality
        val testingBackend = fs2BackendStub.whenRequestMatchesPartial { case r =>
          throw new AssertionError(s"Rejected request reached ${r.uri}")
        }

        (for
          now <- IO.realTimeInstant
          until = now.plusSeconds(60)
          _     = when(repository.reserve(anyString, any[Instant], any[Long], any[Int], any[Option[String]])).thenReturnIO(Some(until))
          access   <- ProviderAccess.make[IO](repository)
          observer <- DatasetAvailabilityObserver.make[IO](availability)
          client   <- FinnhubClient.make[IO](config, testingBackend, access, observer)
          result   <- client.getCompanyProfile(Ticker("AAPL")).attempt
        yield (result, until)).asserting { case (result, until) =>
          result.left.toOption.collect { case error: AppError.ProviderDeferred => error.retryAt } mustBe Some(until)
          verify(repository).reserve(eqTo("Finnhub"), any[Instant], eqTo(1000L), eqTo(0), eqTo(Option.empty[String]))
          verify(availability, never).record(any[Ticker], anyString, anyString, anyString, any[Instant], any[Option[String]])
          succeed
        }
      }

      "pause direct requests after an authentication failure until the shared access is recreated" in {
        val repository     = budgets
        val availability   = quality
        val testingBackend = fs2BackendStub.whenAnyRequest.thenRespondCyclic(
          ResponseStub.adjust("access denied", StatusCode.Unauthorized),
          ResponseStub.adjust(readJson("finnhub/company-profile-success.json"))
        )

        (for
          access          <- ProviderAccess.make[IO](repository)
          observer        <- DatasetAvailabilityObserver.make[IO](availability)
          client          <- FinnhubClient.make[IO](config, testingBackend, access, observer)
          failed          <- client.getCompanyProfile(Ticker("AAPL")).attempt
          paused          <- client.getCompanyProfile(Ticker("AAPL")).attempt
          restartedAccess <- ProviderAccess.make[IO](repository)
          restartedClient <- FinnhubClient.make[IO](config, testingBackend, restartedAccess, observer)
          recovered       <- restartedClient.getCompanyProfile(Ticker("AAPL"))
        yield (failed, paused, recovered)).asserting { case (failed, paused, recovered) =>
          failed.left.toOption.get mustBe a[AppError.ProviderConfiguration]
          paused.left.toOption.get mustBe a[AppError.ProviderConfiguration]
          recovered.map(_.ticker) mustBe Some(Ticker("AAPL"))
          verify(repository, times(2)).reserve(eqTo("Finnhub"), any[Instant], eqTo(1000L), eqTo(0), eqTo(Option.empty[String]))
          verify(availability).record(
            eqTo(Ticker("AAPL")),
            eqTo("Finnhub"),
            eqTo("profile"),
            eqTo("available"),
            any[Instant],
            eqTo(Option.empty[String])
          )
          succeed
        }
      }

      "hold its concurrency permit while a securities response body is being consumed" in {
        val repository = budgets

        (for
          access   <- ProviderAccess.make[IO](repository)
          observer <- DatasetAvailabilityObserver.make[IO](quality)
          started  <- Deferred[IO, Unit]
          release  <- Deferred[IO, Unit]
          body = (Stream.eval(started.complete(())) >> Stream.eval(release.get) >>
            Stream.emit(readJson("finnhub/list-stocks-success.json"))).through(fs2.text.utf8.encode)
          testingBackend = fs2BackendStub.whenAnyRequest.thenRespond(ResponseStub.adjust(body))
          client  <- FinnhubClient.make[IO](config.copy(limits = config.limits.copy(maxConcurrent = 1)), testingBackend, access, observer)
          active  <- client.getListedSecurities(Exchange.NASDAQ).compile.toList.start
          _       <- started.get
          blocked <- client.getCompanyProfile(Ticker("AAPL")).attempt
          _       <- release.complete(())
          securities <- active.joinWithNever
        yield (blocked, securities)).asserting { case (blocked, securities) =>
          blocked.left.toOption.get mustBe a[AppError.ProviderDeferred]
          securities must have size 2
          verify(repository).reserve(eqTo("Finnhub"), any[Instant], eqTo(1000L), eqTo(0), eqTo(Option.empty[String]))
          succeed
        }
      }
    }
  }
}
