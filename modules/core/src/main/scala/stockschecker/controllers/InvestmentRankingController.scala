package stockschecker.controllers

import cats.effect.Async
import org.http4s.HttpRoutes
import stockschecker.common.config.ApiConfig
import stockschecker.domain.*
import stockschecker.services.InvestmentRankingService
import sttp.tapir.*
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce
import sttp.tapir.server.http4s.Http4sServerInterpreter

final private class InvestmentRankingController[F[_]: Async](
    private val service: InvestmentRankingService[F],
    apiKeyRequirement: ApiKeyRequirement
) extends Controller[F](apiKeyRequirement) {
  private val getRankingInputs = secured(InvestmentRankingController.getRankingInputsEndpoint).serverLogic { _ =>
    { case (id, industry, sector, after, limit) =>
      service
        .rankingInputs(id, RankingInputsQuery(industry, sector, after, limit))
        .asResponse
    }
  }

  override val routes: HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(getRankingInputs)
}

object InvestmentRankingController extends TapirJsonCirce with SchemaDerivation {
  private given Schema[FinancialObservation] = Schema.derived[FinancialObservation]
  private given Schema[InvestmentEvidence]   = Schema.derived[InvestmentEvidence]

  private val validRankingId = Validator.custom[String] { id =>
    if id.trim.isEmpty || id.length > 200 then ValidationResult.Invalid("rankingId must contain between 1 and 200 characters")
    else ValidationResult.Valid
  }
  private val validIndustry = Validator.custom[Option[String]] { industry =>
    if industry.exists(value => value.trim.isEmpty || value.length > 200) then
      ValidationResult.Invalid("industry must contain between 1 and 200 characters")
    else ValidationResult.Valid
  }

  private val getRankingInputsEndpoint = Controller.secureEndpoint.get
    .in("investment-rankings" / path[String]("rankingId").validate(validRankingId).map(InvestmentRankingId(_))(_.value) / "inputs")
    .in(query[Option[String]]("industry").validate(validIndustry))
    .in(query[Option[Sector]]("sector"))
    .in(query[Option[Int]]("after").validateOption(Validator.min(0)))
    .in(query[Int]("limit").default(50).validate(Validator.inRange(1, 100)))
    .out(jsonBody[RankingInputsPage])
    .description("Read a bounded page of frozen ranking inputs for administrative diagnosis")

  def make[F[_]: Async](service: InvestmentRankingService[F], config: ApiConfig): F[Controller[F]] =
    Async[F].pure(InvestmentRankingController[F](service, ApiKeyRequirement.Required(config.key)))
}
