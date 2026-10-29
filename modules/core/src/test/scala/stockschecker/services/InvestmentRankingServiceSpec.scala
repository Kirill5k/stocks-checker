package stockschecker.services

import cats.effect.IO
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.*
import stockschecker.fixtures.*
import stockschecker.repositories.*

import java.time.{Instant, LocalDate}

class InvestmentRankingServiceSpec extends IOWordSpec:
  private val now     = Instant.parse("2026-10-19T12:00:00Z")
  private val date    = LocalDate.parse("2026-10-19")
  private val recipe  = InvestmentRecipe()
  private val ranking = InvestmentRanking(InvestmentRankingId("published"), now, recipe, InvestmentRankingSummary(0, 0, Map.empty))

  private class Dependencies(at: Instant = now):
    val rankings = mock[InvestmentRankingRepository[IO]]

    def service: IO[InvestmentRankingService[IO]] =
      given Clock[IO] = Clock.mock(at)
      InvestmentRankingService.make(rankings)

    def allowPruning(): Unit =
      when(rankings.prune(any[Instant])).thenReturnUnit
      ()

  "InvestmentRankingService" should {
    "reuse a ranking from the same UTC day and recipe without loading its entries" in {
      val d        = new Dependencies
      val existing = ranking.copy(createdAt = Instant.parse("2026-10-19T00:00:00Z"))
      when(d.rankings.latest).thenReturnSome(existing)
      d.service.flatMap(_.refresh).asserting { _ =>
        verify(d.rankings).latest
        verifyNoMoreInteractions(d.rankings)
        succeed
      }
    }

    "use its own clock's UTC day just before midnight" in {
      val d        = new Dependencies(Instant.parse("2026-10-19T23:59:59Z"))
      val existing = ranking.copy(createdAt = Instant.parse("2026-10-19T00:00:00Z"))
      when(d.rankings.latest).thenReturnSome(existing)
      d.service.flatMap(_.refresh).asserting { _ =>
        verify(d.rankings).latest
        verifyNoMoreInteractions(d.rankings)
        succeed
      }
    }

    "publish a new ranking when its clock reaches the next UTC day" in {
      val midnight = Instant.parse("2026-10-20T00:00:00Z")
      val d        = new Dependencies(midnight)
      when(d.rankings.latest).thenReturnSome(ranking.copy(createdAt = midnight.minusMillis(1)))
      when(d.rankings.findCandidates).thenReturnIO(Nil)
      when(d.rankings.save(any[InvestmentRanking], any[List[InvestmentRankingEntry]])).thenReturnUnit
      d.allowPruning()
      d.service.flatMap(_.refresh).asserting { _ =>
        val published = org.mockito.ArgumentCaptor.forClass(classOf[InvestmentRanking])
        verify(d.rankings).save(published.capture(), org.mockito.ArgumentMatchers.eq(List.empty[InvestmentRankingEntry]))
        published.getValue.createdAt mustBe midnight
        verify(d.rankings).prune(midnight.minusSeconds(3L * 86400))
        succeed
      }
    }

    List(
      "no published ranking"                  -> Option.empty[InvestmentRanking],
      "a ranking from the previous UTC day"   -> Some(ranking.copy(createdAt = Instant.parse("2026-10-18T23:59:59Z"))),
      "a ranking with another recipe version" -> Some(ranking.copy(recipe = recipe.copy(version = "previous")))
    ).foreach { case (condition, existing) =>
      s"score the complete universe and compact its inputs before storage when there is $condition" in {
        val d           = new Dependencies
        val observation = FinancialObservation(FinancialMetric.FreeCashFlowMargin, date.minusYears(1), BigDecimal("0.2"))
        val financial   = FinancialSnapshot(
          MSFT,
          Provider.Finnhub,
          now,
          Map(
            FinancialMetric.PriceToEarningsTtm     -> BigDecimal(10),
            FinancialMetric.PriceToFreeCashFlowTtm -> BigDecimal(12),
            FinancialMetric.EpsTtm                 -> BigDecimal(4),
            FinancialMetric.RevenueGrowth5Y        -> BigDecimal(999)
          ),
          List(observation),
          List(observation),
          "hash",
          "mapping",
          Nil
        )
        when(d.rankings.latest).thenReturn(IO.pure(existing))
        when(d.rankings.findCandidates)
          .thenReturnIO(List(InvestmentCandidate(AAPLStock, None), InvestmentCandidate(MSFTStock, Some(financial))))
        when(d.rankings.save(any[InvestmentRanking], any[List[InvestmentRankingEntry]])).thenReturnUnit
        d.allowPruning()
        d.service.flatMap(_.refresh).asserting { _ =>
          val published = org.mockito.ArgumentCaptor.forClass(classOf[InvestmentRanking])
          val captured  = org.mockito.ArgumentCaptor.forClass(classOf[List[InvestmentRankingEntry]])
          val ordered   = org.mockito.Mockito.inOrder(d.rankings)
          ordered.verify(d.rankings).save(published.capture(), captured.capture())
          ordered.verify(d.rankings).prune(now.minusSeconds(3L * 86400))
          val refreshed = published.getValue
          val saved     = captured.getValue
          refreshed.id.value must not be empty
          refreshed.createdAt mustBe now
          refreshed.recipe mustBe recipe
          saved.map(_.input) mustBe List(AAPLStock, MSFTStock).map(stock =>
            InvestmentScreenInput(
              stock.security.exchange,
              stock.profile.map(_.country),
              stock.profile.map(_.industry),
              stock.profile.flatMap(_.sector),
              stock.priceAnalytics.map(_.scores.overallScore),
              None
            )
          )
          val evidence = saved.find(_.scorecard.ticker == MSFT).flatMap(_.scorecard.evidence)
          evidence.map(_.provider) mustBe Some(financial.provider)
          evidence.map(_.observedAt) mustBe Some(financial.observedAt)
          evidence.map(_.payloadHash) mustBe Some(financial.payloadHash)
          evidence.map(_.mappingVersion) mustBe Some(financial.mappingVersion)
          refreshed.summary mustBe InvestmentRankingSummary.from(saved)
          succeed
        }
      }
    }

    "skip pruning when saving the ranking fails" in {
      val d       = new Dependencies
      val failure = new RuntimeException("Input batch failed")
      when(d.rankings.latest).thenReturnNone
      when(d.rankings.findCandidates).thenReturnIO(List(InvestmentCandidate(AAPLStock, None)))
      when(d.rankings.save(any[InvestmentRanking], any[List[InvestmentRankingEntry]])).thenReturn(IO.raiseError(failure))
      d.service.flatMap(_.refresh).attempt.asserting { result =>
        result mustBe Left(failure)
        verify(d.rankings, never).prune(any[Instant])
        succeed
      }
    }

  }
