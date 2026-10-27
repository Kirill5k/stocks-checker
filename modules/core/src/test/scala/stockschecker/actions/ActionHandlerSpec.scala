package stockschecker.actions

import cats.effect.{IO, Ref}
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.*
import stockschecker.services.{InvestmentEvaluationService, InvestmentScreenService, Services}

class ActionHandlerSpec extends IOWordSpec:
  "An action handler" should {
    "generate the scheduled screen before capturing evaluations" in {
      val services    = mock[Services[IO]]
      val screens     = mock[InvestmentScreenService[IO]]
      val evaluations = mock[InvestmentEvaluationService[IO]]
      when(services.investmentScreens).thenReturn(screens)
      when(services.investmentEvaluations).thenReturn(evaluations)
      (for {
        steps <- Ref.of[IO, List[String]](Nil)
        _ = when(screens.generateScheduledScreen).thenReturn(steps.update(_ :+ "screen").as(Option.empty[InvestmentScreenSummary]))
        _ = when(evaluations.captureMonthlyEvaluations).thenReturn(steps.update(_ :+ "evaluations").as(1))
        handler  <- ActionHandler.make(services)
        result   <- handler.execute(Action.RefreshInvestmentResearch)
        recorded <- steps.get
      } yield (result, recorded)).asserting { case (result, steps) =>
        result mustBe ActionResult.Completed
        steps mustBe List("screen", "evaluations")
      }
    }

    "skip evaluation capture if scheduled screen generation fails" in {
      val services    = mock[Services[IO]]
      val screens     = mock[InvestmentScreenService[IO]]
      val evaluations = mock[InvestmentEvaluationService[IO]]
      val error       = new IllegalStateException("ranking unavailable")
      when(services.investmentScreens).thenReturn(screens)
      when(services.investmentEvaluations).thenReturn(evaluations)
      when(screens.generateScheduledScreen).thenRaiseError(error)
      (for {
        handler <- ActionHandler.make(services)
        result  <- handler.execute(Action.RefreshInvestmentResearch).attempt
      } yield result).asserting { result =>
        result mustBe Left(error)
        verifyNoInteractions(evaluations)
        succeed
      }
    }
  }
