package stockschecker.actions

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.traverse.*
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.*
import stockschecker.services.{
  CompanyProfileService,
  FinancialMetricsService,
  InvestmentEvaluationService,
  InvestmentRankingService,
  InvestmentScreenService,
  PriceService,
  SecurityService,
  Services
}
import stockschecker.fixtures.*

class ActionHandlerSpec extends IOWordSpec:
  "An action handler" should {
    "expand a purge with ordered research deletes and independent owner deletes" in {
      val services = mock[Services[IO]]
      ActionHandler.make(services).flatMap(_.execute(Action.PurgeStock(AAPL, ts))).asserting { result =>
        result mustBe ActionResult.Expand(
          JobPlan(
            List(
              Action.Sequence(
                NonEmptyList.of(Action.DeleteRankingInputs(AAPL), Action.DeleteScreenEvaluations(AAPL), Action.DeleteScreens(AAPL, ts))
              ),
              Action.DeleteCompanyProfile(AAPL),
              Action.DeletePriceData(AAPL),
              Action.DeleteFinancialData(AAPL)
            ),
            sequential = false
          )
        )
      }
    }

    "freeze affected ranking IDs before deleting screens and releasing their rankings" in {
      val services = mock[Services[IO]]
      val screens  = mock[InvestmentScreenService[IO]]
      val ids      = List(InvestmentRankingId("first"), InvestmentRankingId("second"))
      when(services.investmentScreens).thenReturn(screens)
      when(screens.rankingIds(AAPL)).thenReturnIO(ids)
      ActionHandler.make(services).flatMap(_.execute(Action.DeleteScreens(AAPL, ts))).asserting { result =>
        result mustBe ActionResult.Expand(
          JobPlan(
            Action.DeleteScreenRecords(AAPL) :: ids.map(Action.ReleaseInvestmentRanking(_, ts)),
            sequential = true
          )
        )
        verify(screens).rankingIds(AAPL)
        verify(screens, never).delete(any[Ticker])
        succeed
      }
    }

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
      when(financials.fetch(AAPL)).thenReturnIO(AAPLFinancialMetrics)
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
        verify(financials).fetch(AAPL)
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
      when(security.setActive(ticker, true, ts, "user-excluded")).thenReturnUnit
      when(security.setActive(ticker, false, ts, "user-excluded")).thenReturnUnit
      val events = List(
        Action.RecordCompanyProfileUpdate(tickers),
        Action.RecordPriceAnalyticsUpdate(tickers),
        Action.RecordFinancialMetricsUpdate(tickers),
        Action.SetSecurityActive(ticker, true, ts),
        Action.SetSecurityActive(ticker, false, ts)
      )
      (for {
        handler <- ActionHandler.make(services)
        results <- events.traverse(handler.execute)
      } yield results).asserting { results =>
        results mustBe List.fill(events.size)(ActionResult.Completed)
        verify(security).recordCompanyProfileUpdate(tickers)
        verify(profiles).recordPriceAnalyticsUpdate(tickers)
        verify(profiles).recordFinancialMetricsUpdate(tickers)
        verify(security).setActive(ticker, true, ts, "user-excluded")
        verify(security).setActive(ticker, false, ts, "user-excluded")
        verifyNoMoreInteractions(security, profiles)
        succeed
      }
    }

    "expand research into a ranking-screen sequence and an independent evaluation capture" in {
      val services    = mock[Services[IO]]
      val rankings    = mock[InvestmentRankingService[IO]]
      val screens     = mock[InvestmentScreenService[IO]]
      val evaluations = mock[InvestmentEvaluationService[IO]]
      when(services.investmentRankings).thenReturn(rankings)
      when(services.investmentScreens).thenReturn(screens)
      when(services.investmentEvaluations).thenReturn(evaluations)
      (for
        handler <- ActionHandler.make(services)
        result  <- handler.execute(Action.RefreshInvestmentResearch)
      yield result).asserting { result =>
        result mustBe ActionResult.Expand(
          JobPlan(
            List(
              Action.Sequence(NonEmptyList.of(Action.RefreshInvestmentRanking, Action.CreateMonthlyScreen)),
              Action.CaptureMonthlyEvaluations
            ),
            sequential = false
          )
        )
        verifyNoInteractions(rankings, screens, evaluations)
        succeed
      }
    }

  }
