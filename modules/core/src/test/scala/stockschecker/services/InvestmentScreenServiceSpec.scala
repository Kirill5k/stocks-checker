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

  private def memoryScreens(
      state: Ref[IO, List[InvestmentScreen]],
      value: InvestmentRanking,
      entries: List[InvestmentRankingEntry]
  ): InvestmentScreenRepository[IO] =
    new InvestmentScreenRepository[IO]:
      def save(screen: InvestmentScreen): IO[Unit]                                           = state.update(_ :+ screen)
      def findWithRanking(id: InvestmentScreenId): IO[(InvestmentScreen, InvestmentRanking)] =
        get(id).flatMap(screen => IO.fromOption(screen)(AppError.EntityDoesNotExist("Investment screen", id.value))).map(_ -> value)
      def latestRanking: IO[Option[InvestmentRanking]]                              = IO.pure(Some(value))
      def rankingEntries(id: InvestmentRankingId): IO[List[InvestmentRankingEntry]] = IO.pure(if id == value.id then entries else Nil)
      def get(id: InvestmentScreenId): IO[Option[InvestmentScreen]]                 = state.get.map(_.find(_.id == id))
      def list(limit: Int): IO[List[InvestmentScreenSummary]]                       = state.get.map(_.reverse.take(limit).map(_.summary))

  private def ranking(entries: List[InvestmentRankingEntry]): InvestmentRanking =
    InvestmentRanking(InvestmentRankingId("published"), now, InvestmentRecipe(), InvestmentRankingSummary.from(entries))

  private def paired(inputs: List[InvestmentCandidate], cards: List[InvestmentScorecard]): List[InvestmentRankingEntry] =
    val byTicker = inputs.map(c => c.stock.security.ticker -> c).toMap
    cards.map(card => InvestmentRankingEntry.from(byTicker(card.ticker), card))

  private def readSaved(request: InvestmentScreenRequest, entries: List[InvestmentRankingEntry]): IO[InvestmentScreenResult] =
    val value  = ranking(entries)
    val screen = InvestmentScreen.create(InvestmentScreenId("saved"), now, value, request, Nil)
    for
      state   <- Ref.of[IO, List[InvestmentScreen]](List(screen))
      service <- InvestmentScreenService.make(memoryScreens(state, value, entries))
      found   <- service.get(screen.id)
    yield found

  "InvestmentScreenService" should {
    "propagate a missing screen or ranking from the joined repository read" in {
      val screens = mock[InvestmentScreenRepository[IO]]
      val id      = InvestmentScreenId("missing")
      val error   = AppError.EntityDoesNotExist("Investment screen", id.value)
      when(screens.findWithRanking(id)).thenRaiseError(error)
      InvestmentScreenService.make(screens).flatMap(_.get(id)).attempt.asserting { result =>
        result mustBe Left(error)
        verify(screens).findWithRanking(id)
        verifyNoMoreInteractions(screens)
        succeed
      }
    }

    "read a saved screen using its pinned ranking even after that ranking becomes stale" in {
      val screens = mock[InvestmentScreenRepository[IO]]
      val value   = ranking(Nil).copy(createdAt = now.minusSeconds(10L * 86400))
      val saved   = InvestmentScreen.create(InvestmentScreenId("saved"), value.createdAt, value, InvestmentScreenRequest(), Nil)
      when(screens.findWithRanking(saved.id)).thenReturnIO(saved -> value)
      when(screens.rankingEntries(value.id)).thenReturnIO(Nil)
      InvestmentScreenService.make(screens).flatMap(_.get(saved.id)).asserting { result =>
        result.rankingId mustBe value.id
        verify(screens).findWithRanking(saved.id)
        verify(screens).rankingEntries(value.id)
        verifyNoMoreInteractions(screens)
        succeed
      }
    }

    List(
      InvestmentScreenRequest(limit = Some(0)),
      InvestmentScreenRequest(limit = Some(101)),
      InvestmentScreenRequest(minScore = Some(101)),
      InvestmentScreenRequest(recipeVersion = Some("unknown"))
    ).foreach { request =>
      s"validate $request before reading rankings" in {
        val screens = mock[InvestmentScreenRepository[IO]]
        InvestmentScreenService.make(screens).flatMap(_.create(request)).attempt.asserting { result =>
          result.left.toOption.get mustBe a[AppError.FailedValidation]
          verifyNoInteractions(screens)
          succeed
        }
      }
    }

    List(
      "missing"             -> Option.empty[InvestmentRanking],
      "from another recipe" -> Some(ranking(Nil).copy(recipe = InvestmentRecipe(version = "previous"))),
      "stale"               -> Some(ranking(Nil).copy(createdAt = now.minusSeconds(2L * 86400 + 1)))
    ).foreach { case (condition, value) =>
      s"reject a new screen when the latest ranking is $condition" in {
        val screens = mock[InvestmentScreenRepository[IO]]
        when(screens.latestRanking).thenReturn(IO.pure(value))
        InvestmentScreenService.make(screens).flatMap(_.create(InvestmentScreenRequest())).attempt.asserting { result =>
          result.left.toOption.get mustBe a[AppError.FailedValidation]
          verify(screens).latestRanking
          verifyNoMoreInteractions(screens)
          succeed
        }
      }
    }

    "accept a user-created screen exactly at the ranking freshness boundary" in {
      val screens = mock[InvestmentScreenRepository[IO]]
      val value   = ranking(Nil).copy(createdAt = now.minusSeconds(2L * 86400))
      when(screens.latestRanking).thenReturnSome(value)
      when(screens.rankingEntries(value.id)).thenReturnIO(Nil)
      when(screens.save(any[InvestmentScreen])).thenReturnUnit
      InvestmentScreenService.make(screens).flatMap(_.create(InvestmentScreenRequest())).asserting(_.rankingId mustBe value.id)
    }

    "require today's ranking before creating the monthly screen" in {
      val screens = mock[InvestmentScreenRepository[IO]]
      val id      = InvestmentScreenId("monthly:2026-10:quality-fair-value-v1")
      when(screens.get(id)).thenReturnNone
      when(screens.latestRanking).thenReturnSome(ranking(Nil).copy(createdAt = Instant.parse("2026-10-18T23:59:59Z")))
      InvestmentScreenService.make(screens).flatMap(_.createMonthlyScreen).attempt.asserting { result =>
        result mustBe Left(AppError.FailedValidation("The latest completed ranking is not from today; wait for the scheduled ranking run"))
        verify(screens).get(id)
        verify(screens).latestRanking
        verifyNoMoreInteractions(screens)
        succeed
      }
    }

    "skip freezing a monthly cohort when too few companies are eligible" in {
      val screens = mock[InvestmentScreenRepository[IO]]
      val value   = ranking(Nil)
      val id      = InvestmentScreenId("monthly:2026-10:quality-fair-value-v1")
      when(screens.latestRanking).thenReturnSome(value)
      when(screens.get(id)).thenReturnNone
      InvestmentScreenService.make(screens).flatMap(_.createMonthlyScreen).asserting { result =>
        result mustBe None
        verify(screens).latestRanking
        verify(screens).get(id)
        verifyNoMoreInteractions(screens)
        succeed
      }
    }

    List(
      "missing"               -> Option.empty[InvestmentRanking],
      "from another recipe"   -> Some(ranking(Nil).copy(recipe = InvestmentRecipe(version = "previous"))),
      "stale"                 -> Some(ranking(Nil).copy(createdAt = now.minusSeconds(2L * 86400 + 1))),
      "from the previous day" -> Some(ranking(Nil).copy(createdAt = Instant.parse("2026-10-18T23:59:59Z")))
    ).foreach { case (condition, latest) =>
      s"skip an existing monthly screen without reading a ranking that is $condition" in {
        val screens = mock[InvestmentScreenRepository[IO]]
        val value   = ranking(Nil)
        val id      = InvestmentScreenId("monthly:2026-10:quality-fair-value-v1")
        val saved   = InvestmentScreen.create(id, now, value, InvestmentScreenRequest(), Nil)
        when(screens.latestRanking).thenReturnIO(latest)
        when(screens.get(id)).thenReturnSome(saved)
        InvestmentScreenService.make(screens).flatMap(_.createMonthlyScreen).asserting { result =>
          result mustBe None
          verify(screens).get(id)
          verifyNoMoreInteractions(screens)
          succeed
        }
      }
    }

    "propagate ranking read failures before saving a monthly screen" in {
      val screens = mock[InvestmentScreenRepository[IO]]
      val error   = new IllegalStateException("Ranking storage is unavailable")
      val id      = InvestmentScreenId("monthly:2026-10:quality-fair-value-v1")
      when(screens.get(id)).thenReturnNone
      when(screens.latestRanking).thenRaiseError(error)
      InvestmentScreenService.make(screens).flatMap(_.createMonthlyScreen).attempt.asserting { result =>
        result mustBe Left(error)
        verify(screens).get(id)
        verify(screens).latestRanking
        verifyNoMoreInteractions(screens)
        succeed
      }
    }

    "reference the complete ranking before applying country or display limits and retain exclusions" in {
      val inputs  = List(AAPLStock, MSFTStock).map(s => InvestmentCandidate(s, None))
      val entries = paired(inputs, InvestmentScorer.rank(inputs, now))
      val value   = ranking(entries)
      (for
        state   <- Ref.of[IO, List[InvestmentScreen]](Nil)
        service <- InvestmentScreenService.make(memoryScreens(state, value, entries))
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
        InvestmentRankingEntry.from(
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
      val value = ranking(entries)
      (for
        state   <- Ref.of[IO, List[InvestmentScreen]](Nil)
        service <- InvestmentScreenService.make(memoryScreens(state, value, entries))
        user    <- service.create(InvestmentScreenRequest(limit = Some(1)))
        first   <- service.createMonthlyScreen
        second  <- service.createMonthlyScreen
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
        InvestmentRankingEntry.from(InvestmentCandidate(AAPLStock.copy(security = AAPLSecurity.copy(ticker = c.ticker)), None), c)
      )
      readSaved(InvestmentScreenRequest(minScore = Some(70), minQuality = Some(70)), entries).asserting { found =>
        found.candidates.map(_.ticker) mustBe List(AAPL)
        found.nearMisses.map(_.scorecard.ticker) mustBe List(MSFT)
        found.exclusions.map(_.ticker) mustBe List(missing)
      }
    }

    "show one failed intrinsic financial criterion without pretending it has an investment score" in {
      val annual = (1 to 5).toList.map(n =>
        FinancialObservation(
          FinancialMetric.FreeCashFlowMargin,
          LocalDate.of(2026 - n, 12, 31),
          if n <= 2 then BigDecimal("-0.01") else BigDecimal("0.05")
        )
      )
      val financial = FinancialSnapshot("facts", AAPL, Provider.Finnhub, now, Map.empty, Nil, Nil, "hash", "mapping", Nil)
      val evidence  = InvestmentEvidence(
        Provider.Finnhub,
        now,
        "mapping",
        "hash",
        Map(FinancialMetric.FreeCashFlowMargin -> annual.map(_.period)),
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
      val entries = List(InvestmentRankingEntry.from(InvestmentCandidate(AAPLStock, Some(financial)), card))
      readSaved(InvestmentScreenRequest(), entries).asserting { found =>
        found.candidates mustBe Nil
        found.nearMisses.head.scorecard.score mustBe None
        found.nearMisses.head.failedRule must include("3/5 positive years")
        found.exclusions.map(_.ticker) mustBe List(AAPL)
      }
    }
  }
