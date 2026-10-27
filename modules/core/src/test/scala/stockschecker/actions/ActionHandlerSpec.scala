package stockschecker.actions

import cats.data.NonEmptyList
import cats.effect.{IO, Ref}
import cats.syntax.traverse.*
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.*
import stockschecker.services.{
  CompanyProfileService,
  FinancialMetricsService,
  InvestmentEvaluationService,
  InvestmentScreenService,
  PriceService,
  SecurityService,
  Services
}
import stockschecker.fixtures.*

class ActionHandlerSpec extends IOWordSpec:
  "An action handler" should {
    "execute single-ticker updates and expand batches without calling providers" in {
      val services   = mock[Services[IO]]
      val profiles   = mock[CompanyProfileService[IO]]
      val prices     = mock[PriceService[IO]]
      val financials = mock[FinancialMetricsService[IO]]
      when(services.companyProfile).thenReturn(profiles)
      when(services.price).thenReturn(prices)
      when(services.financialMetrics).thenReturn(financials)
      when(profiles.get(AAPL, true)).thenReturnIO(AAPLCompanyProfile)
      when(prices.findPriceAnalytics(AAPL, true)).thenReturnIO(AAPLPriceAnalytics)
      when(financials.find(AAPL, true)).thenReturnIO(AAPLFinancialMetrics)
      val updates: List[NonEmptyList[Ticker] => Action] = List(
        Action.UpdateCompanyProfiles(_),
        Action.UpdatePriceAnalytics(_),
        Action.UpdateFinancialMetrics(_)
      )
      (for
        handler <- ActionHandler.make(services)
        single  <- updates.traverse(action => handler.execute(action(NonEmptyList.one(AAPL))))
        batches <- updates.traverse(action => handler.execute(action(NonEmptyList.of(AAPL, MSFT))))
      yield (single, batches)).asserting { case (single, batches) =>
        single mustBe List.fill(3)(ActionResult.Completed)
        batches mustBe updates.map(action =>
          ActionResult.Expand(JobPlan(List(action(NonEmptyList.one(AAPL)), action(NonEmptyList.one(MSFT))), sequential = false))
        )
        verify(profiles).get(AAPL, true)
        verify(prices).findPriceAnalytics(AAPL, true)
        verify(financials).find(AAPL, true)
        verifyNoMoreInteractions(profiles, prices, financials)
        succeed
      }
    }

    "deliver internal events to the owning service" in {
      val services = mock[Services[IO]]
      val security = mock[SecurityService[IO]]
      val profiles = mock[CompanyProfileService[IO]]
      val ticker   = Ticker("AAPL")
      val tickers  = NonEmptyList.of(ticker, Ticker("MSFT"))
      when(services.security).thenReturn(security)
      when(services.companyProfile).thenReturn(profiles)
      when(security.recordCompanyProfileUpdate(tickers)).thenReturnUnit
      when(profiles.recordPriceAnalyticsUpdate(tickers)).thenReturnUnit
      when(profiles.recordFinancialMetricsUpdate(tickers)).thenReturnUnit
      when(security.setActive(ticker, true, ts)).thenReturnUnit
      when(profiles.setActive(ticker, true, ts)).thenReturnUnit
      when(security.setActive(ticker, false, ts)).thenReturnUnit
      when(profiles.setActive(ticker, false, ts)).thenReturnUnit
      val events = List(
        Action.RecordCompanyProfileUpdate(tickers),
        Action.RecordPriceAnalyticsUpdate(tickers),
        Action.RecordFinancialMetricsUpdate(tickers),
        Action.SetSecurityActive(ticker, true, ts),
        Action.SetCompanyProfileActive(ticker, true, ts),
        Action.SetSecurityActive(ticker, false, ts),
        Action.SetCompanyProfileActive(ticker, false, ts)
      )
      (for {
        handler <- ActionHandler.make(services)
        results <- events.traverse(handler.execute)
      } yield results).asserting { results =>
        results mustBe List.fill(events.size)(ActionResult.Completed)
        verify(security).recordCompanyProfileUpdate(tickers)
        verify(profiles).recordPriceAnalyticsUpdate(tickers)
        verify(profiles).recordFinancialMetricsUpdate(tickers)
        verify(security).setActive(ticker, true, ts)
        verify(profiles).setActive(ticker, true, ts)
        verify(security).setActive(ticker, false, ts)
        verify(profiles).setActive(ticker, false, ts)
        verifyNoMoreInteractions(security, profiles)
        succeed
      }
    }

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

    "capture evaluations even if scheduled screen generation fails" in {
      val services    = mock[Services[IO]]
      val screens     = mock[InvestmentScreenService[IO]]
      val evaluations = mock[InvestmentEvaluationService[IO]]
      val error       = new IllegalStateException("ranking unavailable")
      when(services.investmentScreens).thenReturn(screens)
      when(services.investmentEvaluations).thenReturn(evaluations)
      when(screens.generateScheduledScreen).thenRaiseError(error)
      when(evaluations.captureMonthlyEvaluations).thenReturnIO(1)
      (for {
        handler <- ActionHandler.make(services)
        result  <- handler.execute(Action.RefreshInvestmentResearch).attempt
      } yield result).asserting { result =>
        result mustBe Left(error)
        verify(evaluations).captureMonthlyEvaluations
        succeed
      }
    }
  }
