package stockschecker.services

import cats.effect.{IO, Ref}
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.repositories.*
import stockschecker.fixtures.*
import java.time.{Instant, LocalDate}

class InvestmentScreenServiceSpec extends IOWordSpec:
  private val now = Instant.parse("2026-10-19T12:00:00Z")
  given Clock[IO] = Clock.mock(now)

  private def memoryScreens(state: Ref[IO, List[InvestmentScreenRun]]): InvestmentScreenRepository[IO] =
    new InvestmentScreenRepository[IO]:
      def save(run: InvestmentScreenRun): IO[Unit]              = state.update(_ :+ run)
      def find(id: InvestmentScreenId): IO[InvestmentScreenRun] =
        state.get.flatMap(runs => IO.fromOption(runs.find(_.id == id))(AppError.EntityDoesNotExist("Investment screen", id.value)))
      def header(id: InvestmentScreenId): IO[Option[InvestmentScreenRepository.Header]] = state.get
        .map(_.find(_.id == id))
        .map(
          _.map(r =>
            InvestmentScreenRepository.Header(r.id, r.createdAt, r.rankingId, r.request, r.cohorts, InvestmentScreenRepository.summary(r))
          )
        )
      def list(limit: Int): IO[List[InvestmentScreenSummary]] = state.get.map(_.reverse.take(limit).map(InvestmentScreenRepository.summary))
      def ids: IO[List[InvestmentScreenId]]                   = state.get.map(_.map(_.id))

  "InvestmentScreenService" should {
    "propagate a missing-screen error from the repository" in {
      val screens     = mock[InvestmentScreenRepository[IO]]
      val rankings    = mock[InvestmentRankingService[IO]]
      val evaluations = mock[InvestmentEvaluationService[IO]]
      val id          = InvestmentScreenId("missing")
      val error       = AppError.EntityDoesNotExist("Investment screen", id.value)
      when(screens.find(id)).thenRaiseError(error)
      (for
        service <- InvestmentScreenService.make(screens, rankings, evaluations)
        result  <- service.get(id)
      yield result).attempt.asserting { result =>
        result mustBe Left(error)
        verify(screens).find(id)
        verifyNoInteractions(rankings, evaluations)
        succeed
      }
    }

    "publish the daily ranking before capturing monthly evaluations during a research refresh" in {
      val screens     = mock[InvestmentScreenRepository[IO]]
      val rankings    = mock[InvestmentRankingService[IO]]
      val evaluations = mock[InvestmentEvaluationService[IO]]
      val header      = InvestmentRankingRepository.Header("daily", now, InvestmentRecipe(), InvestmentRankingSummary(1, 0, Map.empty))
      val id          = InvestmentScreenId("monthly:2026-10:quality-fair-value-v1")
      when(rankings.refresh(LocalDate.parse("2026-10-19"))).thenReturnIO(header)
      when(screens.header(id)).thenReturnNone
      when(evaluations.captureMonthlyEvaluations).thenReturnIO(0)
      val result = for
        service <- InvestmentScreenService.make(screens, rankings, evaluations)
        _       <- service.refreshResearch
      yield ()
      result.asserting { _ =>
        val ordered = org.mockito.Mockito.inOrder(rankings, screens, evaluations)
        ordered.verify(rankings).refresh(LocalDate.parse("2026-10-19"))
        ordered.verify(screens).header(id)
        ordered.verify(evaluations).captureMonthlyEvaluations
        verify(rankings, never).get(any[String])
        verify(screens, never).save(any[InvestmentScreenRun])
        succeed
      }
    }
    "propagate ranking failures and skip evaluation capture during a research refresh" in {
      val screens     = mock[InvestmentScreenRepository[IO]]
      val rankings    = mock[InvestmentRankingService[IO]]
      val evaluations = mock[InvestmentEvaluationService[IO]]
      val error       = new IllegalStateException("Ranking storage is unavailable")
      when(rankings.refresh(any[LocalDate])).thenReturn(IO.raiseError(error))
      val result = for
        service <- InvestmentScreenService.make(screens, rankings, evaluations)
        outcome <- service.refreshResearch.attempt
      yield outcome
      result.asserting { outcome =>
        outcome mustBe Left(error)
        verifyNoInteractions(screens, evaluations)
        succeed
      }
    }
    "snapshot the complete universe before applying country or display limits and retain exclusions" in {
      val rankings = mock[InvestmentRankingService[IO]]
      val inputs   = List(AAPLStock, MSFTStock).map(s => InvestmentCandidate(s, None))
      val ranking  = InvestmentRankingRun("completed-rank", now, InvestmentRecipe(), inputs, InvestmentScorer.rank(inputs, now))
      when(rankings.latest).thenReturnIO(ranking)
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenRun]](Nil)
        service <- InvestmentScreenService.make(
          memoryScreens(state),
          rankings,
          mock[InvestmentEvaluationService[IO]]
        )
        created <- service.create(InvestmentScreenRequest(limit = Some(1), country = Some("GB")))
        saved   <- state.get
      yield (created, saved)
      result.asserting { case (created, saved) =>
        saved.head.inputs.map(_.stock.security.ticker).toSet mustBe Set(AAPL, MSFT)
        saved.head.scorecards.size mustBe 2
        created.summary.universeCount mustBe 2
        created.summary.excludedByReason("missing-financial-snapshot") mustBe 2
        created.candidates mustBe Nil
        saved.head.rankingId mustBe ranking.id
      }
    }
    "report near misses separately from unusable inputs and preserve the requested score floor" in {
      val cards = List(
        InvestmentScorecard(AAPL, true, Some(80), Map.empty, Map.empty, Nil, Nil, Some("peer"), 20, None, "v1"),
        InvestmentScorecard(MSFT, true, Some(60), Map.empty, Map.empty, Nil, Nil, Some("peer"), 20, None, "v1")
      )
      val run = InvestmentScreenRun(
        InvestmentScreenId("saved"),
        now,
        InvestmentRecipe(),
        InvestmentScreenRequest(minScore = Some(70)),
        List(InvestmentCandidate(AAPLStock, None), InvestmentCandidate(MSFTStock, None)),
        cards,
        Nil
      )
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
        service <- InvestmentScreenService.make(
          memoryScreens(state),
          mock[InvestmentRankingService[IO]],
          mock[InvestmentEvaluationService[IO]]
        )
        found <- service.get(run.id)
      yield found
      result.asserting { found =>
        found.candidates.map(_.ticker) mustBe List(AAPL)
        found.nearMisses.map(_.scorecard.ticker) mustBe List(MSFT)
        found.nearMisses.head.failedRule must include("below required score 70")
        found.exclusions mustBe Nil
      }
    }
    "freeze a monthly top twenty independently of user display limits and only once per month" in {
      val inputs = (1 to 25).toList.map { n =>
        InvestmentCandidate(AAPLStock.copy(security = AAPLStock.security.copy(ticker = Ticker(f"TEST$n%02d"))), None)
      }
      val cards = inputs.zipWithIndex.map { case (input, n) =>
        InvestmentScorecard(
          input.stock.security.ticker,
          true,
          Some(BigDecimal(100 - n)),
          Map("quality" -> BigDecimal(n), "value" -> BigDecimal(100 - n)),
          Map.empty,
          Nil,
          Nil,
          Some("peer"),
          25,
          None,
          "quality-fair-value-v1"
        )
      }
      val rank     = InvestmentRankingRun("published", now, InvestmentRecipe(), inputs, cards)
      val rankings = mock[InvestmentRankingService[IO]]
      when(rankings.latest).thenReturnIO(rank)
      when(rankings.refresh(any[LocalDate])).thenReturnIO(
        InvestmentRankingRepository.Header(rank.id, now, rank.recipe, InvestmentRankingRepository.summary(rank))
      )
      when(rankings.get(rank.id)).thenReturnIO(rank)
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenRun]](Nil)
        service <- InvestmentScreenService.make(
          memoryScreens(state),
          rankings,
          mock[InvestmentEvaluationService[IO]]
        )
        user   <- service.create(InvestmentScreenRequest(limit = Some(1)))
        first  <- service.generateScheduledScreen
        second <- service.generateScheduledScreen
        saved  <- state.get
      yield (user, first, second, saved)
      result.asserting { case (user, first, second, saved) =>
        user.candidates.size mustBe 1
        user.nearMisses mustBe Nil
        user.cohorts.find(_.name == "investment").get.tickers.size mustBe 20
        val concentration = user.cohorts.find(_.name == "investment").get.concentration.get
        concentration.constituentCount mustBe 20
        concentration.maxPositionWeightPercent mustBe BigDecimal(5)
        concentration.positionHerfindahl mustBe BigDecimal("0.05")
        concentration.industryWeightsPercent.values.sum mustBe BigDecimal(100)
        first.map(_.id) mustBe Some(InvestmentScreenId("monthly:2026-10:quality-fair-value-v1"))
        second mustBe None
        saved.size mustBe 2
        saved.forall(_.rankingId == rank.id) mustBe true
      }
    }
    "only return eligible companies failing exactly one investment criterion as near misses" in {
      val extra   = Ticker("EXTRA")
      val missing = Ticker("MISSING")
      val card    = InvestmentScorecard(
        AAPL,
        true,
        Some(90),
        Map("quality" -> BigDecimal(90)),
        Map.empty,
        Nil,
        Nil,
        Some("peer"),
        20,
        None,
        "quality-fair-value-v1"
      )
      val cards = List(
        card,
        card.copy(ticker = MSFT, score = Some(60)),
        card.copy(ticker = extra, score = Some(50), pillars = Map("quality" -> BigDecimal(30))),
        card.copy(ticker = missing, eligible = false, score = None, exclusions = List("missing-financial-snapshot"))
      )
      val inputs = cards.map(c => InvestmentCandidate(AAPLStock.copy(security = AAPLStock.security.copy(ticker = c.ticker)), None))
      val run    = InvestmentScreenRun(
        InvestmentScreenId("criteria"),
        now,
        InvestmentRecipe(),
        InvestmentScreenRequest(minScore = Some(70), minQuality = Some(70)),
        inputs,
        cards,
        Nil
      )
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
        service <- InvestmentScreenService.make(
          memoryScreens(state),
          mock[InvestmentRankingService[IO]],
          mock[InvestmentEvaluationService[IO]]
        )
        found <- service.get(run.id)
      yield found
      result.asserting { found =>
        found.candidates.map(_.ticker) mustBe List(AAPL)
        found.nearMisses.map(_.scorecard.ticker) mustBe List(MSFT)
        found.exclusions.map(_.ticker) mustBe List(missing)
      }
    }
    "only read ranking headers when the daily ranking and monthly screen already exist" in {
      val ranking  = InvestmentRankingRepository.Header("published", now, InvestmentRecipe(), InvestmentRankingSummary(100, 25, Map.empty))
      val rankings = mock[InvestmentRankingService[IO]]
      val screens  = mock[InvestmentScreenRepository[IO]]
      val id       = InvestmentScreenId("monthly:2026-10:quality-fair-value-v1")
      val header   = InvestmentScreenRepository.Header(
        id,
        now,
        ranking._id,
        InvestmentScreenRequest(),
        Nil,
        InvestmentScreenSummary(id, now, ranking.recipe.version, 100, 25, 20, Map.empty)
      )
      when(rankings.refresh(any[LocalDate])).thenReturnIO(ranking)
      when(screens.header(id)).thenReturnSome(header)
      val result = for
        service <- InvestmentScreenService.make(
          screens,
          rankings,
          mock[InvestmentEvaluationService[IO]]
        )
        created <- service.generateScheduledScreen
      yield created
      result.asserting { created =>
        created mustBe None
        verify(rankings, never).latest
        verify(rankings, never).get(any[String])
        verify(screens, never).find(any[InvestmentScreenId])
        succeed
      }
    }
    "show one failed intrinsic financial criterion without pretending it has an investment score" in {
      val annual = (1 to 5).toList.map(n =>
        FinancialObservation(
          "fcfMargin",
          LocalDate.of(2026 - n, 12, 31),
          if n <= 2 then BigDecimal("-0.01") else BigDecimal("0.05"),
          "ratio"
        )
      )
      val financial = FinancialSnapshot("facts", AAPL, "Finnhub", now, Map.empty, Nil, Nil, "", "hash", "mapping", Nil)
      val evidence  = InvestmentEvidence(
        "Finnhub",
        now,
        "mapping",
        "hash",
        Map("fcfMargin" -> annual.map(_.period)),
        annual,
        None,
        None,
        Some("USD")
      )
      val card = InvestmentScorecard(
        AAPL,
        false,
        None,
        Map.empty,
        Map.empty,
        List("insufficient-cash-flow-durability"),
        Nil,
        None,
        20,
        Some("facts"),
        "quality-fair-value-v1",
        evidence = Some(evidence)
      )
      val run = InvestmentScreenRun(
        InvestmentScreenId("intrinsic"),
        now,
        InvestmentRecipe(),
        InvestmentScreenRequest(),
        List(InvestmentCandidate(AAPLStock, Some(financial))),
        List(card),
        Nil
      )
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
        service <- InvestmentScreenService.make(
          memoryScreens(state),
          mock[InvestmentRankingService[IO]],
          mock[InvestmentEvaluationService[IO]]
        )
        found <- service.get(run.id)
      yield found
      result.asserting { found =>
        found.candidates mustBe Nil
        found.nearMisses.head.scorecard.score mustBe None
        found.nearMisses.head.failedRule must include("3/5 positive years")
        found.exclusions.map(_.ticker) mustBe List(AAPL)
      }
    }
  }
