package stockschecker.services

import cats.effect.{IO, Ref}
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.domain.investment.InvestmentScorer
import stockschecker.repositories.*
import stockschecker.fixtures.*

import java.time.{Instant, LocalDate}

class InvestmentScreenServiceSpec extends IOWordSpec:
  private val now = Instant.parse("2026-10-19T12:00:00Z")
  given Clock[IO] = Clock.mock(now)

  private def memoryScreens(state: Ref[IO, List[InvestmentScreen]]): InvestmentScreenRepository[IO] =
    new InvestmentScreenRepository[IO]:
      def save(screen: InvestmentScreen): IO[Unit]           = state.update(_ :+ screen)
      def find(id: InvestmentScreenId): IO[InvestmentScreen] =
        state.get.flatMap(screens => IO.fromOption(screens.find(_.id == id))(AppError.EntityDoesNotExist("Investment screen", id.value)))
      def get(id: InvestmentScreenId): IO[Option[InvestmentScreen]] = state.get.map(_.find(_.id == id))
      def list(limit: Int): IO[List[InvestmentScreenSummary]]       = state.get.map(_.reverse.take(limit).map(_.summary))
      def ids: IO[List[InvestmentScreenId]]                         = state.get.map(_.map(_.id))
      def rankingIds: IO[Set[InvestmentRankingId]]                  = state.get.map(_.map(_.rankingId).toSet)

  private def ranking(entries: List[InvestmentRankingEntry]): InvestmentRanking =
    InvestmentRanking(InvestmentRankingId("published"), now, InvestmentRecipe(), InvestmentRankingSummary.from(entries))

  private def paired(inputs: List[InvestmentCandidate], cards: List[InvestmentScorecard]): List[InvestmentRankingEntry] =
    val byTicker = inputs.map(c => c.stock.security.ticker -> c).toMap
    cards.map(card => InvestmentRankingEntry(byTicker(card.ticker), card))

  private def publishedRanking(value: InvestmentRanking, entries: List[InvestmentRankingEntry]): InvestmentRankingService[IO] =
    val rankings = mock[InvestmentRankingService[IO]]
    when(rankings.latest).thenReturnIO(value)
    when(rankings.find(value.id)).thenReturnSome(value)
    when(rankings.entries(value.id)).thenReturnIO(entries)
    rankings

  private def readSaved(request: InvestmentScreenRequest, entries: List[InvestmentRankingEntry]): IO[InvestmentScreenResult] =
    val value  = ranking(entries)
    val screen = InvestmentScreen.create(InvestmentScreenId("saved"), now, value, request, Nil)
    for
      state   <- Ref.of[IO, List[InvestmentScreen]](List(screen))
      service <- InvestmentScreenService.make(memoryScreens(state), publishedRanking(value, entries))
      found   <- service.get(screen.id)
    yield found

  "InvestmentScreenService" should {
    "propagate a missing-screen error from the repository" in {
      val screens  = mock[InvestmentScreenRepository[IO]]
      val rankings = mock[InvestmentRankingService[IO]]
      val id       = InvestmentScreenId("missing")
      val error    = AppError.EntityDoesNotExist("Investment screen", id.value)
      when(screens.find(id)).thenRaiseError(error)
      InvestmentScreenService.make(screens, rankings).flatMap(_.get(id)).attempt.asserting { result =>
        result mustBe Left(error)
        verifyNoInteractions(rankings)
        succeed
      }
    }

    "return a missing-screen error when its referenced ranking has disappeared" in {
      val screens  = mock[InvestmentScreenRepository[IO]]
      val rankings = mock[InvestmentRankingService[IO]]
      val id       = InvestmentScreenId("dangling")
      val value    = ranking(Nil)
      val screen   = InvestmentScreen.create(id, now, value, InvestmentScreenRequest(), Nil)
      when(screens.find(id)).thenReturnIO(screen)
      when(rankings.find(value.id)).thenReturnNone
      InvestmentScreenService.make(screens, rankings).flatMap(_.get(id)).attempt.asserting { result =>
        result mustBe Left(AppError.EntityDoesNotExist("Investment screen", id.value))
        verify(rankings, never).entries(any[InvestmentRankingId])
        succeed
      }
    }

    List(
      InvestmentScreenRequest(limit = Some(0)),
      InvestmentScreenRequest(limit = Some(101)),
      InvestmentScreenRequest(minScore = Some(101)),
      InvestmentScreenRequest(recipeVersion = Some("unknown"))
    ).foreach { request =>
      s"validate $request before requesting a ranking" in {
        val screens  = mock[InvestmentScreenRepository[IO]]
        val rankings = mock[InvestmentRankingService[IO]]
        InvestmentScreenService.make(screens, rankings).flatMap(_.create(request)).attempt.asserting { result =>
          result.left.toOption.get mustBe a[AppError.FailedValidation]
          verifyNoInteractions(rankings, screens)
          succeed
        }
      }
    }

    "refresh the daily ranking without freezing a cohort when too few companies are eligible" in {
      val screens  = mock[InvestmentScreenRepository[IO]]
      val rankings = mock[InvestmentRankingService[IO]]
      val value    = ranking(Nil)
      val id       = InvestmentScreenId("monthly:2026-10:quality-fair-value-v1")
      when(rankings.refresh(LocalDate.parse("2026-10-19"))).thenReturnIO(value)
      when(screens.get(id)).thenReturnNone
      InvestmentScreenService.make(screens, rankings).flatMap(_.generateScheduledScreen).asserting { result =>
        result mustBe None
        val ordered = org.mockito.Mockito.inOrder(rankings, screens)
        ordered.verify(rankings).refresh(LocalDate.parse("2026-10-19"))
        ordered.verify(screens).get(id)
        verify(rankings, never).entries(any[InvestmentRankingId])
        verify(screens, never).save(any[InvestmentScreen])
        succeed
      }
    }

    "propagate ranking failures before saving a scheduled screen" in {
      val screens  = mock[InvestmentScreenRepository[IO]]
      val rankings = mock[InvestmentRankingService[IO]]
      val error    = new IllegalStateException("Ranking storage is unavailable")
      when(rankings.refresh(any[LocalDate])).thenReturn(IO.raiseError(error))
      InvestmentScreenService.make(screens, rankings).flatMap(_.generateScheduledScreen).attempt.asserting { result =>
        result mustBe Left(error)
        verifyNoInteractions(screens)
        succeed
      }
    }

    "reference the complete ranking before applying country or display limits and retain exclusions" in {
      val inputs   = List(AAPLStock, MSFTStock).map(s => InvestmentCandidate(s, None))
      val entries  = paired(inputs, InvestmentScorer.rank(inputs, now))
      val value    = ranking(entries)
      val rankings = publishedRanking(value, entries)
      (for
        state   <- Ref.of[IO, List[InvestmentScreen]](Nil)
        service <- InvestmentScreenService.make(memoryScreens(state), rankings)
        created <- service.create(InvestmentScreenRequest(limit = Some(1), country = Some("GB")))
        saved   <- state.get
      yield (created, saved)).asserting { case (created, saved) =>
        saved.head.summary mustBe created.summary
        created.summary.universeCount mustBe 2
        created.summary.excludedByReason("missing-financial-snapshot") mustBe 2
        created.candidates mustBe Nil
        saved.head.rankingId mustBe value.id
      }
    }

    "report near misses separately from unusable inputs and preserve the requested score floor" in {
      val cards = List(
        InvestmentScorecard(AAPL, true, Some(80), Map.empty, Map.empty, Nil, Nil, Some("peer"), 20, None, "v1"),
        InvestmentScorecard(MSFT, true, Some(60), Map.empty, Map.empty, Nil, Nil, Some("peer"), 20, None, "v1")
      )
      val entries = paired(List(InvestmentCandidate(AAPLStock, None), InvestmentCandidate(MSFTStock, None)), cards)
      readSaved(InvestmentScreenRequest(minScore = Some(70)), entries).asserting { found =>
        found.candidates.map(_.ticker) mustBe List(AAPL)
        found.nearMisses.map(_.scorecard.ticker) mustBe List(MSFT)
        found.nearMisses.head.failedRule must include("below required score 70")
        found.exclusions mustBe Nil
      }
    }

    "freeze a monthly top twenty independently of user display limits and only once per month" in {
      val entries = (1 to 25).toList.map { n =>
        val ticker = Ticker(f"TEST$n%02d")
        InvestmentRankingEntry(
          InvestmentCandidate(AAPLStock.copy(security = AAPLSecurity.copy(ticker = ticker)), None),
          InvestmentScorecard(
            ticker,
            true,
            Some(BigDecimal(100 - n)),
            Map(Pillar.Quality -> BigDecimal(n), Pillar.Value -> BigDecimal(100 - n)),
            Map.empty,
            Nil,
            Nil,
            Some("peer"),
            25,
            None,
            "quality-fair-value-v1"
          )
        )
      }
      val value    = ranking(entries)
      val rankings = publishedRanking(value, entries)
      when(rankings.refresh(any[LocalDate])).thenReturnIO(value)
      (for
        state   <- Ref.of[IO, List[InvestmentScreen]](Nil)
        service <- InvestmentScreenService.make(memoryScreens(state), rankings)
        user    <- service.create(InvestmentScreenRequest(limit = Some(1)))
        first   <- service.generateScheduledScreen
        second  <- service.generateScheduledScreen
        saved   <- state.get
      yield (user, first, second, saved)).asserting { case (user, first, second, saved) =>
        user.candidates.size mustBe 1
        user.nearMisses mustBe Nil
        user.cohorts.find(_.name == CohortName.Investment).get.tickers.size mustBe 20
        val concentration = user.cohorts.find(_.name == CohortName.Investment).get.concentration.get
        concentration.constituentCount mustBe 20
        concentration.maxPositionWeightPercent mustBe BigDecimal(5)
        concentration.positionHerfindahl mustBe BigDecimal("0.05")
        concentration.industryWeightsPercent.values.sum mustBe BigDecimal(100)
        first.map(_.id) mustBe Some(InvestmentScreenId("monthly:2026-10:quality-fair-value-v1"))
        second mustBe None
        saved.size mustBe 2
        saved.forall(_.rankingId == value.id) mustBe true
      }
    }

    "only return eligible companies failing exactly one investment criterion as near misses" in {
      val extra   = Ticker("EXTRA")
      val missing = Ticker("MISSING")
      val card    =
        InvestmentScorecard(AAPL, true, Some(90), Map(Pillar.Quality -> BigDecimal(90)), Map.empty, Nil, Nil, Some("peer"), 20, None, "v1")
      val cards = List(
        card,
        card.copy(ticker = MSFT, score = Some(60)),
        card.copy(ticker = extra, score = Some(50), pillars = Map(Pillar.Quality -> BigDecimal(30))),
        card.copy(ticker = missing, eligible = false, score = None, exclusions = List("missing-financial-snapshot"))
      )
      val entries = cards.map(c =>
        InvestmentRankingEntry(InvestmentCandidate(AAPLStock.copy(security = AAPLSecurity.copy(ticker = c.ticker)), None), c)
      )
      readSaved(InvestmentScreenRequest(minScore = Some(70), minQuality = Some(70)), entries).asserting { found =>
        found.candidates.map(_.ticker) mustBe List(AAPL)
        found.nearMisses.map(_.scorecard.ticker) mustBe List(MSFT)
        found.exclusions.map(_.ticker) mustBe List(missing)
      }
    }

    "only read metadata when the daily ranking and monthly screen already exist" in {
      val value    = ranking(Nil).copy(summary = InvestmentRankingSummary(100, 25, Map.empty))
      val rankings = mock[InvestmentRankingService[IO]]
      val screens  = mock[InvestmentScreenRepository[IO]]
      val id       = InvestmentScreenId("monthly:2026-10:quality-fair-value-v1")
      val screen   = InvestmentScreen.create(id, now, value, InvestmentScreenRequest(), Nil)
      when(rankings.refresh(any[LocalDate])).thenReturnIO(value)
      when(screens.get(id)).thenReturnSome(screen)
      InvestmentScreenService.make(screens, rankings).flatMap(_.generateScheduledScreen).asserting { result =>
        result mustBe None
        verify(rankings, never).latest
        verify(rankings, never).entries(any[InvestmentRankingId])
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
      val financial = FinancialSnapshot("facts", AAPL, Provider.Finnhub, now, Map.empty, Nil, Nil, "", "hash", "mapping", Nil)
      val evidence  = InvestmentEvidence(
        Provider.Finnhub,
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
      val entries = List(InvestmentRankingEntry(InvestmentCandidate(AAPLStock, Some(financial)), card))
      readSaved(InvestmentScreenRequest(), entries).asserting { found =>
        found.candidates mustBe Nil
        found.nearMisses.head.scorecard.score mustBe None
        found.nearMisses.head.failedRule must include("3/5 positive years")
        found.exclusions.map(_.ticker) mustBe List(AAPL)
      }
    }
  }
