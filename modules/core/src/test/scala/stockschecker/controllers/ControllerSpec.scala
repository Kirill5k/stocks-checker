package stockschecker.controllers

import cats.effect.IO
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.http4s.*
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import stockschecker.common.config.ApiConfig
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.services.*

class ControllerSpec extends HttpRoutesWordSpec {
  private val config                                                                                      = ApiConfig("test-key")
  private val commandId                                                                                   = "64b000000000000000000001"
  private val factories: List[(String, () => (IO[Controller[IO]], List[AnyRef]), List[(Method, String)])] = List(
    (
      "security",
      () => { val s = mock[SecurityService[IO]]; (SecurityController.make(s, config), List(s)) },
      List(Method.GET -> "/securities/AAPL", Method.GET -> "/securities/tickers", Method.GET -> "/securities/exchange/nasdaq")
    ),
    (
      "profile",
      () => { val s = mock[CompanyProfileService[IO]]; (CompanyProfileController.make(s, config), List(s)) },
      List(Method.GET -> "/company-profiles", Method.GET -> "/company-profiles/AAPL")
    ),
    (
      "price",
      () => { val s = mock[PriceService[IO]]; (PriceController.make(s, config), List(s)) },
      List(Method.GET -> "/price/performance-summaries", Method.GET -> "/price/performance-summaries/AAPL")
    ),
    (
      "stock",
      () => { val s = mock[StockService[IO]]; (StockController.make(s, config), List(s)) },
      List(
        Method.GET -> "/stocks",
        Method.GET -> "/stocks/AAPL",
        Method.PUT -> "/stocks/AAPL/activate",
        Method.PUT -> "/stocks/AAPL/deactivate"
      )
    ),
    (
      "command",
      () => { val s = mock[CommandService[IO]]; (CommandController.make(config, s), List(s)) },
      List(
        Method.GET  -> "/commands",
        Method.POST -> "/commands",
        Method.PUT  -> s"/commands/$commandId",
        Method.PUT  -> s"/commands/$commandId/active",
        Method.POST -> s"/commands/$commandId/execute"
      )
    ),
    (
      "jobs",
      () => { val s = mock[JobService[IO]]; (JobController.make(s, config), List(s)) },
      List(Method.GET -> "/jobs", Method.GET -> "/jobs/status?id=run%2F0")
    ),
    (
      "quality",
      () => { val s = mock[DataQualityService[IO]]; (DataQualityController.make(s, config), List(s)) },
      List(Method.GET -> "/data-quality")
    ),
    (
      "ranking",
      () => { val s = mock[InvestmentRankingService[IO]]; (InvestmentRankingController.make(s, config), List(s)) },
      List(Method.GET -> "/investment-rankings/test/inputs")
    ),
    (
      "screen",
      () => {
        val s = mock[InvestmentScreenService[IO]]; val e = mock[InvestmentEvaluationService[IO]];
        (InvestmentScreenController.make(s, e, config), List(s, e))
      },
      List(
        Method.GET  -> "/investment-screens",
        Method.POST -> "/investment-screens",
        Method.GET  -> "/investment-screens/test",
        Method.GET  -> "/investment-screens/test/evaluation"
      )
    )
  )

  "Secure endpoints" should
    factories.foreach { case (name, make, endpoints) =>
      endpoints.foreach { case (method, path) =>
        List(None, Some("invalid-key")).foreach { key =>
          s"reject ${key.fold("missing")(_ => "invalid")} credentials for $name $method $path" in {
            val (controller, services) = make()
            val request                = Request[IO](method = method, uri = Uri.unsafeFromString(path))
            val authenticated          = key.fold(request)(value => request.withHeaders(Header.Raw(CIString("X-API-Key"), value)))
            val response               = for
              c      <- controller
              result <- c.routes.orNotFound.run(authenticated)
              _      <- IO(services.foreach(s => verifyNoInteractions(s)))
            yield result
            response mustHaveStatus (Status.Unauthorized, Some("""{"message":"Invalid API key"}"""))
          }
        }
      }
    }

  "HTTP error responses" should
    List[Throwable](
      new RuntimeException("token=private"),
      new IllegalArgumentException("private server failure"),
      new NumberFormatException("private server failure"),
      AppError.JsonParsingFailure("private response", "private parsing detail"),
      AppError.HttpClient("provider", 500, "private response"),
      AppError.ProviderConfiguration(Provider.Finnhub, "private credentials")
    )
      .foreach { error =>
        s"hide internal details from ${error.getClass.getSimpleName}" in {
          val service = mock[SecurityService[IO]]
          when(service.find(Ticker("AAPL"))).thenRaiseError(error)
          val result = for
            controller <- SecurityController.make(service, config)
            response   <- controller.routes.orNotFound.run(
              Request[IO](uri = uri"/securities/AAPL")
                .withHeaders(Header.Raw(CIString("X-API-Key"), config.key))
            )
            body <- response.as[String]
          yield
            response.status.code mustBe (if error.isInstanceOf[AppError.ProviderConfiguration] then 503 else 500)
            body must not include "private"
          result.unsafeToFuture()(using cats.effect.unsafe.IORuntime.global)
        }
      }

  "Request decoding" should {
    "return 422 for a malformed numeric query without exposing its value or invoking the service" in {
      val service  = mock[JobService[IO]]
      val response = for
        controller <- JobController.make(service, config)
        result     <- controller.routes.orNotFound.run(
          Request[IO](uri = uri"/jobs?limit=private-invalid-number")
            .withHeaders(Header.Raw(CIString("X-API-Key"), config.key))
        )
      yield result
      response mustHaveStatus (Status.UnprocessableContent, Some("""{"message":"Invalid value for: query parameter limit"}"""))
      verifyNoInteractions(service)
    }
  }
}
