package stockschecker.clients.finnhub

import stockschecker.domain.{Provider, ProviderDeferralReason}
import kirill5k.common.cats.Clock
import cats.effect.{Deferred, IO}
import kirill5k.common.sttp.test.Sttp4WordSpec
import stockschecker.clients.{ProviderAccess, ProviderLimits}
import stockschecker.common.config.FinnhubClientConfig
import stockschecker.domain.{CompanyProfile, Exchange, FinancialMetrics, Sector, Security, SecurityKind, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.ProviderBudgetRepository
import stockschecker.repositories.ProviderBudgetRepository.Deferral
import sttp.client4.testing.ResponseStub
import fs2.Stream
import sttp.model.StatusCode

import java.time.{Instant, LocalDate}

class FinnhubClientSpec extends Sttp4WordSpec {
  given Clock[IO]                                   = Clock.make[IO]
  private def budgets: ProviderBudgetRepository[IO] = {
    val repository = mock[ProviderBudgetRepository[IO]]
    when(repository.reserve(any[Provider], any[Instant], any[Long], any[Int], any[Option[String]])).thenReturnIO(None)
    when(repository.cooldown(any[Provider], any[Instant], any[Option[String]])).thenReturnUnit
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
          client     <- FinnhubClient.make[IO](config, testingBackend, access)
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
          client     <- FinnhubClient.make[IO](config, testingBackend, access)
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
          access  <- providerAccess
          client  <- FinnhubClient.make[IO](config, testingBackend, access)
          profile <- client.getCompanyProfile(Ticker("AAPL"))
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
              marketCap = 3989245252568L,
              sector = Some(Sector.Technology)
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
          access  <- providerAccess
          client  <- FinnhubClient.make[IO](config, testingBackend, access)
          profile <- client.getCompanyProfile(Ticker("INVALID"))
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
          access  <- providerAccess
          client  <- FinnhubClient.make[IO](config, testingBackend, access)
          profile <- client.getCompanyProfile(Ticker("AAPL"))
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
          access  <- providerAccess
          client  <- FinnhubClient.make[IO](config, testingBackend, access)
          metrics <- client.getFinancialData(Ticker("AAPL"))
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
          access  <- providerAccess
          client  <- FinnhubClient.make[IO](config, testingBackend, access)
          metrics <- client.getFinancialData(Ticker("INVALID"))
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
          access  <- providerAccess
          client  <- FinnhubClient.make[IO](config, testingBackend, access)
          profile <- client.getFinancialData(Ticker("AAPL"))
        yield profile

        result.attempt.asserting { response =>
          response.left.toOption.get mustBe a[AppError.ProviderDeferred]
        }
      }
    }

    "response handling" should {
      val requests: List[(String, FinnhubClient[IO] => IO[Unit])] = List(
        "profiles"   -> (_.getCompanyProfile(Ticker("AAPL")).void),
        "financials" -> (_.getFinancialData(Ticker("AAPL")).void),
        "listings"   -> (_.getListedSecurities(Exchange.NASDAQ).compile.drain)
      )
      requests.foreach { case (name, request) =>
        List(StatusCode.NotFound, StatusCode.Unauthorized, StatusCode.Forbidden, StatusCode.TooManyRequests, StatusCode.ServiceUnavailable)
          .foreach { status =>
            s"map HTTP ${status.code} for $name through provider errors" in {
              val backend = fs2BackendStub.whenAnyRequest.thenRespond(ResponseStub.adjust("provider failure", status))
              (for
                access <- providerAccess
                client <- FinnhubClient.make[IO](config, backend, access)
                result <- request(client).attempt
              yield result).asserting { result =>
                val error = result.left.toOption.get
                status match
                  case StatusCode.Unauthorized | StatusCode.Forbidden => error mustBe a[AppError.ProviderConfiguration]
                  case StatusCode.TooManyRequests                     => error mustBe a[AppError.ProviderDeferred]
                  case _                                              =>
                    error mustBe a[AppError.HttpClient]
                    error.asInstanceOf[AppError.HttpClient].status mustBe status.code
                error.getMessage must include(if name == "listings" then "NASDAQ" else "AAPL")
                error.getMessage must include(name match
                  case "profiles"   => "company profile"
                  case "financials" => "basic financials"
                  case _            => "listed securities")
              }
            }
          }
      }

      "retain exchange context when a listed securities body cannot be decoded" in {
        val body    = Stream.emit("not json").through(fs2.text.utf8.encode)
        val backend = fs2BackendStub.whenAnyRequest.thenRespond(ResponseStub.adjust(body))
        (for
          access <- providerAccess
          client <- FinnhubClient.make[IO](config, backend, access)
          result <- client.getListedSecurities(Exchange.NASDAQ).compile.toList.attempt
        yield result).asserting { result =>
          result.left.toOption.get mustBe a[AppError.JsonParsingFailure]
          result.left.toOption.get.getMessage must include("listed securities for NASDAQ")
        }
      }

      "preserve transport failures while consuming a listed securities body" in {
        val failure = new java.io.IOException("connection interrupted")
        val body    = Stream.raiseError[IO](failure)
        val backend = fs2BackendStub.whenAnyRequest.thenRespond(ResponseStub.adjust(body))
        (for
          access <- providerAccess
          client <- FinnhubClient.make[IO](config, backend, access)
          result <- client.getListedSecurities(Exchange.NASDAQ).compile.toList.attempt
        yield result).asserting(_ mustBe Left(failure))
      }

      requests.take(2).foreach { case (name, request) =>
        s"report malformed successful $name responses as decoding failures" in {
          val backend = fs2BackendStub.whenAnyRequest.thenRespond(ResponseStub.adjust("not json"))
          (for
            access <- providerAccess
            client <- FinnhubClient.make[IO](config, backend, access)
            result <- request(client).attempt
          yield result).asserting { result =>
            result.left.toOption.get mustBe a[AppError.JsonParsingFailure]
            result.left.toOption.get.getMessage must include("AAPL")
            result.left.toOption.get.getMessage must include(if name == "profiles" then "company profile" else "basic financials")
          }
        }
      }
    }

    "provider access" should {
      "reject a direct request when its shared budget is exhausted without reaching the provider" in {
        val repository     = budgets
        val testingBackend = fs2BackendStub.whenRequestMatchesPartial { case r =>
          throw new AssertionError(s"Rejected request reached ${r.uri}")
        }

        (for
          now <- IO.realTimeInstant
          until = now.plusSeconds(60)
          _     = when(repository.reserve(any[Provider], any[Instant], any[Long], any[Int], any[Option[String]]))
            .thenReturnIO(Some(Deferral(until, ProviderDeferralReason.DailyQuota)))
          access <- ProviderAccess.make[IO](repository)
          client <- FinnhubClient.make[IO](config, testingBackend, access)
          result <- client.getCompanyProfile(Ticker("AAPL")).attempt
        yield (result, until)).asserting { case (result, until) =>
          result.left.toOption mustBe Some(AppError.ProviderDeferred(Provider.Finnhub, until, ProviderDeferralReason.DailyQuota))
          verify(repository).reserve(eqTo(Provider.Finnhub), any[Instant], eqTo(1000L), eqTo(0), eqTo(Option.empty[String]))
          succeed
        }
      }

      "pause direct requests after an authentication failure until the shared access is recreated" in {
        val repository     = budgets
        val testingBackend = fs2BackendStub.whenAnyRequest.thenRespondCyclic(
          ResponseStub.adjust("access denied", StatusCode.Unauthorized),
          ResponseStub.adjust(readJson("finnhub/company-profile-success.json"))
        )

        (for
          access          <- ProviderAccess.make[IO](repository)
          client          <- FinnhubClient.make[IO](config, testingBackend, access)
          failed          <- client.getCompanyProfile(Ticker("AAPL")).attempt
          paused          <- client.getCompanyProfile(Ticker("AAPL")).attempt
          restartedAccess <- ProviderAccess.make[IO](repository)
          restartedClient <- FinnhubClient.make[IO](config, testingBackend, restartedAccess)
          recovered       <- restartedClient.getCompanyProfile(Ticker("AAPL"))
        yield (failed, paused, recovered)).asserting { case (failed, paused, recovered) =>
          failed.left.toOption.get mustBe a[AppError.ProviderConfiguration]
          paused.left.toOption.get mustBe a[AppError.ProviderConfiguration]
          recovered.map(_.ticker) mustBe Some(Ticker("AAPL"))
          verify(repository, times(2)).reserve(eqTo(Provider.Finnhub), any[Instant], eqTo(1000L), eqTo(0), eqTo(Option.empty[String]))
          succeed
        }
      }

      "hold its concurrency permit while a securities response body is being consumed" in {
        val repository = budgets

        (for
          access  <- ProviderAccess.make[IO](repository)
          started <- Deferred[IO, Unit]
          release <- Deferred[IO, Unit]
          body = (Stream.eval(started.complete(())) >> Stream.eval(release.get) >>
            Stream.emit(readJson("finnhub/list-stocks-success.json"))).through(fs2.text.utf8.encode)
          testingBackend = fs2BackendStub.whenAnyRequest.thenRespond(ResponseStub.adjust(body))
          client     <- FinnhubClient.make[IO](config.copy(limits = config.limits.copy(maxConcurrent = 1)), testingBackend, access)
          active     <- client.getListedSecurities(Exchange.NASDAQ).compile.toList.start
          _          <- started.get
          blocked    <- client.getCompanyProfile(Ticker("AAPL")).attempt
          _          <- release.complete(())
          securities <- active.joinWithNever
        yield (blocked, securities)).asserting { case (blocked, securities) =>
          blocked.left.toOption.get mustBe a[AppError.ProviderDeferred]
          securities must have size 2
          verify(repository).reserve(eqTo(Provider.Finnhub), any[Instant], eqTo(1000L), eqTo(0), eqTo(Option.empty[String]))
          succeed
        }
      }
    }
  }
}
