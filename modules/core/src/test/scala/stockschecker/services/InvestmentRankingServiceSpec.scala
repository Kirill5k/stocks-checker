package stockschecker.services

import cats.effect.{IO, Ref}
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.domain.*
import stockschecker.fixtures.*
import stockschecker.repositories.*

import java.time.{Instant, LocalDate}

class InvestmentRankingServiceSpec extends IOWordSpec:
  private val now    = Instant.parse("2026-10-19T12:00:00Z")
  private val date   = LocalDate.parse("2026-10-19")
  private val recipe = InvestmentRecipe()
  given Clock[IO]    = Clock.mock(now)
  given Logger[IO]   = Slf4jLogger.getLogger[IO]

  private def completeCandidate(n: Int): InvestmentCandidate =
    val ticker = Ticker(f"TEST$n%02d")
    val annual = (0 until 5).toList.flatMap { offset =>
      val period = LocalDate.of(2025 - offset, 12, 31)
      List(
        FinancialObservation(FinancialMetric.ReturnOnAssets, period, BigDecimal("0.1")),
        FinancialObservation(FinancialMetric.FreeCashFlowMargin, period, BigDecimal("0.2")),
        FinancialObservation(FinancialMetric.DebtToAssets, period, BigDecimal("0.3"))
      )
    }
    val snapshot = FinancialSnapshot(
      ticker,
      Provider.Finnhub,
      now,
      Map(
        FinancialMetric.PriceToEarningsTtm     -> BigDecimal(10),
        FinancialMetric.PriceToFreeCashFlowTtm -> BigDecimal(12),
        FinancialMetric.EpsTtm                 -> BigDecimal(4),
        FinancialMetric.RevenueGrowth5Y        -> BigDecimal("0.1"),
        FinancialMetric.EpsGrowth5Y            -> BigDecimal("0.2")
      ),
      annual,
      List(FinancialObservation(FinancialMetric.FreeCashFlowPerShareTtm, date.minusMonths(1), BigDecimal(5))),
      "hash",
      "mapping",
      Nil
    )
    InvestmentCandidate(
      Stock(
        AAPLSecurity.copy(ticker = ticker),
        Some(AAPLCompanyProfile.copy(ticker = ticker, industry = "Technology", sector = Some(Sector.Technology))),
        Some(
          AAPLPriceAnalytics.copy(
            ticker = ticker,
            performanceSummary = AAPLPriceAnalytics.performanceSummary.copy(latestPriceDate = date)
          )
        )
      ),
      Some(snapshot)
    )

  private type Published = List[(InvestmentRanking, List[InvestmentRankingEntry])]

  private def memoryRankings(
      candidates: Ref[IO, List[InvestmentCandidate]],
      published: Ref[IO, Published]
  ): InvestmentRankingRepository[IO] = new InvestmentRankingRepository[IO]:
    def findCandidates: IO[List[InvestmentCandidate]]                                     = candidates.get
    def latest: IO[Option[InvestmentRanking]]                                             = published.get.map(_.headOption.map(_._1))
    def save(ranking: InvestmentRanking, entries: List[InvestmentRankingEntry]): IO[Unit] =
      published.update((ranking -> entries) :: _)
    def prune(before: Instant): IO[Unit] = IO.unit

  private def memoryScreens(
      state: Ref[IO, List[InvestmentScreen]],
      published: Ref[IO, Published]
  ): InvestmentScreenRepository[IO] = new InvestmentScreenRepository[IO]:
    def latestRanking: IO[Option[InvestmentRanking]]                              = published.get.map(_.headOption.map(_._1))
    def rankingEntries(id: InvestmentRankingId): IO[List[InvestmentRankingEntry]] =
      published.get.map(_.find(_._1.id == id).fold(List.empty[InvestmentRankingEntry])(_._2))
    def save(screen: InvestmentScreen): IO[Unit]                                           = state.update(_ :+ screen)
    def get(id: InvestmentScreenId): IO[Option[InvestmentScreen]]                          = state.get.map(_.find(_.id == id))
    def list(limit: Int): IO[List[InvestmentScreenSummary]]                                = state.get.map(_.take(limit).map(_.summary))
    def findWithRanking(id: InvestmentScreenId): IO[(InvestmentScreen, InvestmentRanking)] =
      for
        screen  <- get(id).map(_.get)
        ranking <- published.get.map(_.find(_._1.id == screen.rankingId).get._1)
      yield screen -> ranking

  "InvestmentRankingService" should {
    "rebuild after a same-day refill, create the monthly screen and preserve its pinned ranking on later refreshes" in {
      val complete = (1 to 20).toList.map(completeCandidate)
      (for
        candidates  <- Ref.of[IO, List[InvestmentCandidate]](complete.map(_.copy(financialSnapshot = None)))
        published   <- Ref.of[IO, Published](Nil)
        saved       <- Ref.of[IO, List[InvestmentScreen]](Nil)
        rankings    <- InvestmentRankingService.make(memoryRankings(candidates, published))
        screens     <- InvestmentScreenService.make(memoryScreens(saved, published))
        _           <- rankings.refresh
        first       <- published.get.map(_.head._1)
        skipped     <- screens.createMonthlyScreen
        _           <- candidates.set(complete)
        _           <- rankings.refresh
        recovered   <- published.get.map(_.head._1)
        created     <- screens.createMonthlyScreen
        screen      <- saved.get.map(_.head)
        _           <- rankings.refresh
        repeated    <- screens.createMonthlyScreen
        pinned      <- screens.get(screen.id)
        allRankings <- published.get.map(_.map(_._1))
        allScreens  <- saved.get
      yield (first, skipped, recovered, created, screen, repeated, pinned, allRankings, allScreens)).asserting {
        case (first, skipped, recovered, created, screen, repeated, pinned, allRankings, allScreens) =>
          first.summary.eligibleCount mustBe 0
          skipped mustBe MonthlyScreenOutcome.InsufficientEligible(first.id, 0, InvestmentRecipe.MinimumEligibleStocks)
          recovered.summary.eligibleCount mustBe 20
          created mustBe MonthlyScreenOutcome.Created(recovered.id, screen.summary)
          screen.summary.selectedCount mustBe 20
          repeated mustBe MonthlyScreenOutcome.AlreadyExists(recovered.id, screen.summary)
          pinned.rankingId mustBe recovered.id
          pinned.candidates.size mustBe 20
          allRankings.map(_.id).distinct.size mustBe 3
          allRankings.forall(_.createdAt == now) mustBe true
          allRankings.last mustBe first
          allScreens mustBe List(screen)
      }
    }

    "score the complete universe and compact its inputs before publishing and pruning" in {
      val rankings    = mock[InvestmentRankingRepository[IO]]
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
      when(rankings.findCandidates)
        .thenReturnIO(List(InvestmentCandidate(AAPLStock, None), InvestmentCandidate(MSFTStock, Some(financial))))
      when(rankings.save(any[InvestmentRanking], any[List[InvestmentRankingEntry]])).thenReturnUnit
      when(rankings.prune(any[Instant])).thenReturnUnit
      InvestmentRankingService.make(rankings).flatMap(_.refresh).asserting { _ =>
        val published = org.mockito.ArgumentCaptor.forClass(classOf[InvestmentRanking])
        val captured  = org.mockito.ArgumentCaptor.forClass(classOf[List[InvestmentRankingEntry]])
        val ordered   = org.mockito.Mockito.inOrder(rankings)
        ordered.verify(rankings).findCandidates
        ordered.verify(rankings).save(published.capture(), captured.capture())
        ordered.verify(rankings).prune(now.minusSeconds(3L * 86400))
        verifyNoMoreInteractions(rankings)
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

    "skip pruning when saving the ranking fails" in {
      val rankings = mock[InvestmentRankingRepository[IO]]
      val failure  = new RuntimeException("Input batch failed")
      when(rankings.findCandidates).thenReturnIO(List(InvestmentCandidate(AAPLStock, None)))
      when(rankings.save(any[InvestmentRanking], any[List[InvestmentRankingEntry]])).thenReturn(IO.raiseError(failure))
      InvestmentRankingService.make(rankings).flatMap(_.refresh).attempt.asserting { result =>
        result mustBe Left(failure)
        verify(rankings, never).prune(any[Instant])
        succeed
      }
    }
  }
