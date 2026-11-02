package stockschecker.services

import cats.effect.{IO, Ref}
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.domain.*
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.domain.errors.AppError
import stockschecker.domain.investment.{InvestmentScorer, InvestmentScreenSelection}
import stockschecker.repositories.*
import stockschecker.fixtures.*

import java.time.{Instant, LocalDate}

class InvestmentScreenServiceSpec extends IOWordSpec:
  private val now  = Instant.parse("2026-10-19T12:00:00Z")
  given Clock[IO]  = Clock.mock(now)
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private def make(screens: InvestmentScreenRepository[IO]): IO[InvestmentScreenService[IO]] =
    val dispatcher = mock[ActionDispatcher[IO]]
    when(dispatcher.dispatchOnce(any[JobId], any[Action])).thenReturnUnit
    InvestmentScreenService.make(screens, dispatcher, InvestmentRecipe())

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
      def detailEntries(screen: InvestmentScreen, includeExclusions: Boolean): IO[List[InvestmentRankingEntry]] =
        rankingEntries(screen.rankingId).map { rows =>
          val scoped = rows.filter(entry =>
            screen.request.exchange.forall(_ == entry.input.exchange) &&
              screen.request.country.forall(country => entry.input.country.contains(country))
          )
          if includeExclusions then rows
          else
            val holdings   = screen.cohorts.iterator.flatMap(_.tickers).toSet
            val nearMisses = scoped
              .filter(entry => !entry.scorecard.eligible && entry.input.exclusionExplanation.isDefined)
              .take(InvestmentRecipe.NearMissLimit)
              .map(_.scorecard.ticker)
              .toSet
            rows.filter(entry => holdings(entry.scorecard.ticker) || nearMisses(entry.scorecard.ticker))
        }
      def rankingIds(ticker: Ticker): IO[List[InvestmentRankingId]] =
        state.get.map(_.filter(_.cohorts.exists(_.tickers.contains(ticker))).map(_.rankingId).distinct)
      def delete(ticker: Ticker): IO[Unit] = state.update(_.filterNot(_.cohorts.exists(_.tickers.contains(ticker))))
      def get(id: InvestmentScreenId): IO[Option[InvestmentScreen]] = state.get.map(_.find(_.id == id))
      def list(limit: Int): IO[List[InvestmentScreenSummary]]       = state.get.map(_.reverse.take(limit).map(_.summary))

  private def ranking(entries: List[InvestmentRankingEntry]): InvestmentRanking =
    InvestmentRanking(InvestmentRankingId("published"), now, InvestmentRecipe(), InvestmentRankingSummary.from(entries))

  private def paired(inputs: List[InvestmentCandidate], cards: List[InvestmentScorecard]): List[InvestmentRankingEntry] =
    val byTicker = inputs.map(c => c.stock.security.ticker -> c).toMap
    cards.map(card => InvestmentRankingEntry.from(byTicker(card.ticker), card))

  private def readSaved(
      request: InvestmentScreenRequest,
      entries: List[InvestmentRankingEntry],
      includeExclusions: Boolean = false
  ): IO[InvestmentScreenResult] =
    val value   = ranking(entries)
    val cohorts = InvestmentScreenSelection(entries, request).cohorts
    val screen  = InvestmentScreen.create(InvestmentScreenId("saved"), now, value, request, cohorts)
    for
      state   <- Ref.of[IO, List[InvestmentScreen]](List(screen))
      service <- make(memoryScreens(state, value, entries))
      found   <- service.get(screen.id, includeExclusions)
    yield found

  "InvestmentScreenService" should {
    "omit exclusions by default and return them on request without changing the saved screen or candidate evidence" in {
      val excluded = Ticker("EXCLUDED")
      val card     = InvestmentScorecard(AAPL, true, Some(90), Map.empty, Map.empty, Nil, Nil, Some("peer"), 20, "v1")
      val cards    = List(
        card,
        card.copy(ticker = MSFT, eligible = false, score = None, exclusions = List("missing-financial-snapshot")),
        card.copy(ticker = excluded, eligible = false, score = None, exclusions = List("insufficient-cash-flow-durability"))
      )
      val entries = cards.map { value =>
        val entry = InvestmentRankingEntry.from(
          InvestmentCandidate(AAPLStock.copy(security = AAPLSecurity.copy(ticker = value.ticker)), None),
          value
        )
        if value.ticker == excluded then entry.copy(input = entry.input.copy(exclusionExplanation = Some("Only three positive years")))
        else entry
      }
      val value     = ranking(entries)
      val request   = InvestmentScreenRequest()
      val selection = InvestmentScreenSelection(entries, request)
      val saved     = InvestmentScreen.create(InvestmentScreenId("saved"), now, value, request, selection.cohorts)
      (for
        state    <- Ref.of[IO, List[InvestmentScreen]](List(saved))
        service  <- make(memoryScreens(state, value, entries))
        compact  <- service.get(saved.id)
        full     <- service.get(saved.id, includeExclusions = true)
        retained <- state.get
      yield (compact, full, retained)).asserting { case (compact, full, retained) =>
        compact.exclusions mustBe Nil
        full.exclusions.map(_.ticker) mustBe List(MSFT, excluded)
        compact.candidates mustBe List(card)
        compact.candidates mustBe full.candidates
        compact.nearMisses.map(_.scorecard.ticker) mustBe List(excluded)
        compact.nearMisses mustBe full.nearMisses
        compact.summary mustBe saved.summary
        compact.cohorts mustBe saved.cohorts
        compact.rankingId mustBe saved.rankingId
        full.copy(exclusions = Nil) mustBe compact
        retained mustBe List(saved)
      }
    }

    "retain a saved hundred-stock display limit and threshold near misses outside the investment cohort" in {
      val cards = (1 to 105).toList.map { n =>
        InvestmentScorecard(
          Ticker(f"TEST$n%03d"),
          true,
          Some(if n <= 100 then BigDecimal(90) else BigDecimal(60)),
          Map.empty,
          Map.empty,
          Nil,
          Nil,
          Some("peer"),
          105,
          "v1"
        )
      }
      val entries = cards.map(card =>
        InvestmentRankingEntry.from(
          InvestmentCandidate(AAPLStock.copy(security = AAPLSecurity.copy(ticker = card.ticker)), None),
          card
        )
      )
      readSaved(InvestmentScreenRequest(limit = Some(100), minScore = Some(70)), entries).asserting { result =>
        result.candidates mustBe cards.take(100)
        result.nearMisses.map(_.scorecard) mustBe cards.drop(100)
        result.cohorts.find(_.name == CohortName.Investment).get.tickers mustBe cards.take(20).map(_.ticker)
        result.cohorts.find(_.name == CohortName.EqualUniverse).get.tickers.toSet mustBe cards.map(_.ticker).toSet
        result.exclusions mustBe Nil
      }
    }

    "propagate a missing screen or ranking from the joined repository read" in {
      val screens = mock[InvestmentScreenRepository[IO]]
      val id      = InvestmentScreenId("missing")
      val error   = AppError.EntityDoesNotExist("Investment screen", id.value)
      when(screens.findWithRanking(id)).thenRaiseError(error)
      make(screens).flatMap(_.get(id)).attempt.asserting { result =>
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
      when(screens.detailEntries(saved, false)).thenReturnIO(Nil)
      make(screens).flatMap(_.get(saved.id)).asserting { result =>
        result.rankingId mustBe value.id
        verify(screens).findWithRanking(saved.id)
        verify(screens).detailEntries(saved, false)
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
        make(screens).flatMap(_.create(request)).attempt.asserting { result =>
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
        make(screens).flatMap(_.create(InvestmentScreenRequest())).attempt.asserting { result =>
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
      make(screens).flatMap(_.create(InvestmentScreenRequest())).asserting(_.rankingId mustBe value.id)
    }

    "require today's ranking before creating the monthly screen" in {
      val screens = mock[InvestmentScreenRepository[IO]]
      val id      = InvestmentScreenId("monthly:2026-10:quality-fair-value-v1")
      when(screens.get(id)).thenReturnNone
      when(screens.latestRanking).thenReturnSome(ranking(Nil).copy(createdAt = Instant.parse("2026-10-18T23:59:59Z")))
      make(screens).flatMap(_.createMonthlyScreen).attempt.asserting { result =>
        result mustBe Right(MonthlyScreenOutcome.RankingUnavailable("The latest completed ranking is not from today"))
        verify(screens).get(id)
        verify(screens).latestRanking
        verifyNoMoreInteractions(screens)
        succeed
      }
    }

    List(
      "missing"      -> Option.empty[InvestmentRanking],
      "stale"        -> Some(ranking(Nil).copy(createdAt = now.minusSeconds(3L * 86400))),
      "incompatible" -> Some(ranking(Nil).copy(recipe = InvestmentRecipe(version = "old")))
    ).foreach { case (reason, latest) =>
      s"return a typed skipped outcome when a monthly ranking is $reason" in {
        val screens = mock[InvestmentScreenRepository[IO]]
        when(screens.get(any[InvestmentScreenId])).thenReturnNone
        when(screens.latestRanking).thenReturnIO(latest)
        make(screens).flatMap(_.createMonthlyScreen).asserting { result =>
          result mustBe a[MonthlyScreenOutcome.RankingUnavailable]
          verify(screens, never).save(any[InvestmentScreen])
          succeed
        }
      }
    }

    "dispatch an idempotent ranking pin after saving a manual screen" in {
      val screens    = mock[InvestmentScreenRepository[IO]]
      val dispatcher = mock[ActionDispatcher[IO]]
      val value      = ranking(Nil)
      when(screens.latestRanking).thenReturnSome(value)
      when(screens.rankingEntries(value.id)).thenReturnIO(Nil)
      when(screens.save(any[InvestmentScreen])).thenReturnUnit
      when(dispatcher.dispatchOnce(any[JobId], any[Action])).thenReturnUnit
      InvestmentScreenService.make(screens, dispatcher, InvestmentRecipe()).flatMap(_.create(InvestmentScreenRequest())).asserting {
        result =>
          val ordered = org.mockito.Mockito.inOrder(screens, dispatcher)
          ordered.verify(screens).save(any[InvestmentScreen])
          ordered.verify(dispatcher).dispatchOnce(JobId(s"pin-ranking-${result.summary.id.value}"), Action.PinInvestmentRanking(value.id))
          succeed
      }
    }

    "skip freezing a monthly cohort when too few companies are eligible" in {
      val screens = mock[InvestmentScreenRepository[IO]]
      val value   = ranking(Nil)
      val id      = InvestmentScreenId("monthly:2026-10:quality-fair-value-v1")
      when(screens.latestRanking).thenReturnSome(value)
      when(screens.get(id)).thenReturnNone
      make(screens).flatMap(_.createMonthlyScreen).asserting { result =>
        result mustBe MonthlyScreenOutcome.InsufficientEligible(value.id, 0, InvestmentRecipe.MinimumEligibleStocks)
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
        make(screens).flatMap(_.createMonthlyScreen).asserting { result =>
          result mustBe MonthlyScreenOutcome.AlreadyExists(saved.rankingId, saved.summary)
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
      make(screens).flatMap(_.createMonthlyScreen).attempt.asserting { result =>
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
        service <- make(memoryScreens(state, value, entries))
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
        InvestmentScorecard(AAPL, true, Some(80), Map.empty, Map.empty, Nil, Nil, Some("peer"), 20, "v1"),
        InvestmentScorecard(MSFT, true, Some(60), Map.empty, Map.empty, Nil, Nil, Some("peer"), 20, "v1")
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
            "quality-fair-value-v1"
          )
        )
      }
      val value = ranking(entries)
      (for
        state   <- Ref.of[IO, List[InvestmentScreen]](Nil)
        service <- make(memoryScreens(state, value, entries))
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
        val monthly = saved.find(_.id == InvestmentScreenId("monthly:2026-10:quality-fair-value-v1")).get
        first mustBe MonthlyScreenOutcome.Created(value.id, monthly.summary)
        second mustBe MonthlyScreenOutcome.AlreadyExists(value.id, monthly.summary)
        saved.size mustBe 2
        saved.forall(_.rankingId == value.id) mustBe true
      }
    }

    "only return eligible companies failing exactly one investment criterion as near misses" in {
      val extra   = Ticker("EXTRA")
      val missing = Ticker("MISSING")
      val card    =
        InvestmentScorecard(AAPL, true, Some(90), Map(Pillar.Quality -> BigDecimal(90)), Map.empty, Nil, Nil, Some("peer"), 20, "v1")
      val cards = List(
        card,
        card.copy(ticker = MSFT, score = Some(60)),
        card.copy(ticker = extra, score = Some(50), pillars = Map(Pillar.Quality -> BigDecimal(30))),
        card.copy(ticker = missing, eligible = false, score = None, exclusions = List("missing-financial-snapshot"))
      )
      val entries = cards.map(c =>
        InvestmentRankingEntry.from(InvestmentCandidate(AAPLStock.copy(security = AAPLSecurity.copy(ticker = c.ticker)), None), c)
      )
      readSaved(InvestmentScreenRequest(minScore = Some(70), minQuality = Some(70)), entries, includeExclusions = true).asserting { found =>
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
      val financial = FinancialSnapshot(AAPL, Provider.Finnhub, now, Map.empty, Nil, Nil, "hash", "mapping", Nil)
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
        "quality-fair-value-v1",
        evidence = Some(evidence)
      )
      val entries = List(InvestmentRankingEntry.from(InvestmentCandidate(AAPLStock, Some(financial)), card))
      readSaved(InvestmentScreenRequest(), entries).asserting { found =>
        found.candidates mustBe Nil
        found.nearMisses.head.scorecard.score mustBe None
        found.nearMisses.head.failedRule must include("3/5 positive years")
        found.exclusions mustBe Nil
      }
    }
  }
