package stockschecker.controllers

import cats.effect.Async
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.deriveEncoder
import org.http4s.HttpRoutes
import stockschecker.common.config.ApiConfig
import stockschecker.domain.*
import stockschecker.services.{InvestmentEvaluationService, InvestmentScreenService}
import sttp.tapir.*
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce
import sttp.tapir.server.http4s.Http4sServerInterpreter

final private class InvestmentScreenController[F[_]: Async](
    service: InvestmentScreenService[F],
    evaluations: InvestmentEvaluationService[F],
    apiKeyRequirement: ApiKeyRequirement
) extends Controller[F](apiKeyRequirement):
  import InvestmentScreenController.*
  private val create = secured(createEndpoint).serverLogic(_ => request => service.create(request).asResponse)
  private val get    = secured(getEndpoint).serverLogic { _ =>
    { case (id, includeExclusionsValue) =>
      val includeExclusions = includeExclusionsValue == "true"
      service.get(id, includeExclusions).mapToResponse(InvestmentScreenView.from(_, includeExclusions))
    }
  }
  private val list                   = secured(listEndpoint).serverLogic(_ => limit => service.list(limit.getOrElse(20)).asResponse)
  private val evaluate               = secured(evaluationEndpoint).serverLogic(_ => id => evaluations.evaluate(id).asResponse)
  override val routes: HttpRoutes[F] = Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(List(create, get, list, evaluate))

object InvestmentScreenController extends TapirJsonCirce with SchemaDerivation:
  final private case class InvestmentScreenView(
      summary: InvestmentScreenSummary,
      request: InvestmentScreenRequest,
      candidates: List[InvestmentScorecard],
      nearMisses: List[InvestmentNearMiss],
      exclusions: Option[List[InvestmentScorecard]],
      cohorts: List[InvestmentCohort],
      rankingId: InvestmentRankingId
  ) derives Decoder

  private object InvestmentScreenView:
    given Encoder.AsObject[InvestmentScreenView] = deriveEncoder[InvestmentScreenView].mapJsonObject { fields =>
      if fields("exclusions").exists(_.isNull) then fields.remove("exclusions") else fields
    }

    def from(result: InvestmentScreenResult, includeExclusions: Boolean): InvestmentScreenView =
      InvestmentScreenView(
        result.summary,
        result.request,
        result.candidates,
        result.nearMisses,
        Option.when(includeExclusions)(result.exclusions),
        result.cohorts,
        result.rankingId
      )

  private given Schema[java.time.YearMonth] = Schema.string
  // Derive intermediate schemas explicitly to keep deeply nested evaluation responses within inline limits.
  private given Schema[EvaluationCostScenario]  = Schema.derived[EvaluationCostScenario]
  private given Schema[EvaluationPriceEvidence] = Schema.derived[EvaluationPriceEvidence]
  private given Schema[InvestmentConcentration] = Schema.derived[InvestmentConcentration]
  private given Schema[CohortEvaluation]        = Schema.derived[CohortEvaluation]
  private given Schema[HorizonEvaluation]       = Schema.derived[HorizonEvaluation]
  private given Schema[InvestmentEvaluation]    = Schema.derived[InvestmentEvaluation]
  private given Schema[FinancialObservation]    = Schema.derived[FinancialObservation]
  private given Schema[InvestmentEvidence]      = Schema.derived[InvestmentEvidence]
  private given Schema[InvestmentScorecard]     = Schema.derived[InvestmentScorecard]
  private val base                              = "investment-screens"
  private val includeExclusions                 = query[String]("includeExclusions")
    .default("false")
    .validate(Validator.enumeration(List("true", "false")))
    .description("Include every in-scope excluded scorecard; this can substantially increase response size and query time")
  private val createEndpoint = Controller.secureEndpoint.post
    .in(base)
    .in(jsonBody[InvestmentScreenRequest])
    .out(jsonBody[InvestmentScreenResult])
  private val getEndpoint = Controller.secureEndpoint.get
    .in(base / path[InvestmentScreenId]("id"))
    .in(includeExclusions)
    .out(jsonBody[InvestmentScreenView])
    .description("Read a saved screen using its original request criteria; candidate limits are set in the creation request body")
  private val listEndpoint =
    Controller.secureEndpoint.get.in(base).in(query[Option[Int]]("limit")).out(jsonBody[List[InvestmentScreenSummary]])
  private val evaluationEndpoint =
    Controller.secureEndpoint.get.in(base / path[InvestmentScreenId]("id") / "evaluation").out(jsonBody[InvestmentEvaluation])
  def make[F[_]: Async](
      service: InvestmentScreenService[F],
      evaluations: InvestmentEvaluationService[F],
      config: ApiConfig
  ): F[Controller[F]] =
    Async[F].pure(new InvestmentScreenController(service, evaluations, ApiKeyRequirement.Required(config.key)))
