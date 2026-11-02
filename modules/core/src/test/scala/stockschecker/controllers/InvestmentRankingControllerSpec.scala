package stockschecker.controllers

import cats.effect.IO
import io.circe.syntax.*
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.http4s.{Header, Request, Status}
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import stockschecker.common.config.ApiConfig
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.services.InvestmentRankingService

import java.time.Instant

class InvestmentRankingControllerSpec extends HttpRoutesWordSpec {
  private val config = ApiConfig("test-key")
  private val apiKey = Header.Raw(CIString("X-API-Key"), config.key)

  "Ranking input endpoint" should {
    "require authentication for frozen ranking inputs" in {
      val service  = mock[InvestmentRankingService[IO]]
      val response = for
        controller <- InvestmentRankingController.make(service, config)
        response   <- controller.routes.orNotFound.run(Request[IO](uri = uri"/investment-rankings/rank-1/inputs"))
      yield response
      response mustHaveStatus (Status.Unauthorized, Some("""{"message":"Invalid API key"}"""))
      verifyNoInteractions(service)
    }

    "default ranking input pages to fifty rows" in {
      val service = mock[InvestmentRankingService[IO]]
      val id      = InvestmentRankingId("rank-1")
      val page    = RankingInputsPage(id, Instant.parse("2026-11-02T10:10:45Z"), Nil, None)
      when(service.rankingInputs(id, RankingInputsQuery(limit = 50))).thenReturnIO(page)
      val response = for
        controller <- InvestmentRankingController.make(service, config)
        response   <- controller.routes.orNotFound.run(Request[IO](uri = uri"/investment-rankings/rank-1/inputs").withHeaders(apiKey))
      yield response
      response mustHaveStatus (Status.Ok, Some(page.asJson.noSpaces))
      verify(service).rankingInputs(id, RankingInputsQuery(limit = 50))
    }

    "decode industry, sector and ordinal pagination before passing the query to the service" in {
      val service = mock[InvestmentRankingService[IO]]
      val id      = InvestmentRankingId("rank-1")
      val query   = RankingInputsQuery(Some("  Health Care "), Some(Sector.Healthcare), Some(0), 100)
      val page    = RankingInputsPage(id, Instant.parse("2026-11-02T10:10:45Z"), Nil, None)
      when(service.rankingInputs(id, query)).thenReturnIO(page)
      val uri = uri"/investment-rankings/rank-1/inputs"
        .withQueryParam("industry", "  Health Care ")
        .withQueryParam("sector", "healthcare")
        .withQueryParam("after", 0)
        .withQueryParam("limit", 100)
      val response = for
        controller <- InvestmentRankingController.make(service, config)
        response   <- controller.routes.orNotFound.run(Request[IO](uri = uri).withHeaders(apiKey))
      yield response
      response mustHaveStatus (Status.Ok, Some(page.asJson.noSpaces))
      verify(service).rankingInputs(id, query)
    }

    List(
      ("zero limit", uri"/investment-rankings/rank-1/inputs?limit=0", "Invalid value for: query parameter limit (must satisfy >=1)"),
      (
        "excessive limit",
        uri"/investment-rankings/rank-1/inputs?limit=101",
        "Invalid value for: query parameter limit (must satisfy <=100)"
      ),
      ("negative cursor", uri"/investment-rankings/rank-1/inputs?after=-1", "Invalid value for: query parameter after (must satisfy >=0)"),
      (
        "unknown sector",
        uri"/investment-rankings/rank-1/inputs?sector=unknown",
        "Invalid value unknown for enum Sector, Accepted values: technology,healthcare,industrials,consumer-discretionary,consumer-staples,energy,utilities,materials,communication-services,financials,real-estate"
      ),
      (
        "empty industry",
        uri"/investment-rankings/rank-1/inputs".withQueryParam("industry", " "),
        "industry must contain between 1 and 200 characters"
      )
    ).foreach { case (description, uri, message) =>
      s"reject $description before reading ranking inputs" in {
        val service  = mock[InvestmentRankingService[IO]]
        val response = for
          controller <- InvestmentRankingController.make(service, config)
          response   <- controller.routes.orNotFound.run(Request[IO](uri = uri).withHeaders(apiKey))
        yield response
        response mustHaveStatus (Status.UnprocessableContent, Some(ErrorResponse(message).asJson.noSpaces))
        verifyNoInteractions(service)
      }
    }

    "return not found for unknown or pruned rankings" in {
      val service = mock[InvestmentRankingService[IO]]
      val id      = InvestmentRankingId("missing")
      val error   = AppError.EntityDoesNotExist("Investment ranking", id.value)
      when(service.rankingInputs(id, RankingInputsQuery(limit = 50))).thenRaiseError(error)
      val response = for
        controller <- InvestmentRankingController.make(service, config)
        response   <- controller.routes.orNotFound.run(Request[IO](uri = uri"/investment-rankings/missing/inputs").withHeaders(apiKey))
      yield response
      response mustHaveStatus (Status.NotFound, Some(ErrorResponse(error.getMessage).asJson.noSpaces))
    }

  }
}
