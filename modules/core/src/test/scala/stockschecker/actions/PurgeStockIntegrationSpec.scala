package stockschecker.actions

import cats.effect.IO
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import fs2.Stream
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.Filter
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.clients.{Clients, MarketDataClient}
import stockschecker.common.config.{InvestmentConfig, SecurityConfig}
import stockschecker.domain.*
import stockschecker.domain.investment.InvestmentEvaluator
import stockschecker.fixtures.*
import stockschecker.repositories.*
import stockschecker.repositories.InvestmentRepositoryFixtures.*
import stockschecker.repositories.InvestmentRepositoryFixtures.given
import stockschecker.services.Services

import java.time.{LocalDate, YearMonth}

class PurgeStockIntegrationSpec extends RepositorySpec:
  override val port = 12225
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private val clients = new Clients[IO]:
    val marketData: MarketDataClient[IO] = new MarketDataClient[IO]:
      private def unexpected[A]: IO[A] = IO.raiseError(new IllegalStateException("Purging must not call a provider"))
      def getTradedSecurities(exchange: Exchange): Stream[IO, Security] = Stream.eval(unexpected)
      def getCompanyProfile(ticker: Ticker): IO[Option[CompanyProfile]] = unexpected
      def getFinancialData(ticker: Ticker): IO[Option[FinancialData]] = unexpected
      def getDailyPriceHistory(ticker: Ticker, range: DateRange, adjustment: PriceAdjustment): IO[PriceHistorySnapshot] = unexpected

  private def run(handler: ActionHandler[IO], action: Action): IO[Unit] =
    handler.execute(action).flatMap {
      case ActionResult.Completed => IO.unit
      case ActionResult.Expand(plan) =>
        if plan.sequential then plan.actions.traverse_(run(handler, _))
        else plan.actions.parTraverse_(run(handler, _))
    }

  private def ids(db: MongoDatabase[IO], collection: String): IO[Set[String]] =
    db.getCollection(collection).flatMap(_.find.all).map(_.flatMap(_.getString("_id")).toSet)

  "The purge action chain" should {
    "retain the security tombstone while deleting all owned stock and research data, releasing rankings and preserving unrelated data" in {
      val seed = Map(
        SecurityRepository.CollectionName -> List(toDoc(AAPLSecurity.copy(companyProfileLastUpdatedAt = Some(now))), toDoc(MSFTSecurity)),
        CompanyProfileRepository.CollectionName -> List(toDoc(AAPLCompanyProfile), toDoc(MSFTCompanyProfile)),
        PriceAnalyticsRepository.CollectionName -> List(toDoc(AAPLPriceAnalytics), toDoc(AAPLPriceAnalytics.copy(ticker = MSFT))),
        FinancialMetricsRepository.CollectionName -> List(toDoc(AAPLFinancialMetrics), toDoc(AAPLFinancialMetrics.copy(ticker = MSFT)))
      )
      val retainedEntries = List(entries.head.copy(scorecard = card.copy(ticker = MSFT)))
      val retainedRanking = ranking.copy(id = InvestmentRankingId("retained-ranking"))
      val affected = screen.copy(createdAt = now.minusSeconds(90L * 86400), cohorts = List(
        InvestmentCohort(CohortName.Investment, List(MSFT)),
        InvestmentCohort(CohortName.EqualUniverse, List(AAPL, MSFT))
      ))
      val retainedScreen = screen.copy(
        id = InvestmentScreenId("retained-screen"),
        createdAt = affected.createdAt,
        rankingId = retainedRanking.id,
        cohorts = List(InvestmentCohort(CohortName.EqualUniverse, List(MSFT))),
        summary = screen.summary.copy(id = InvestmentScreenId("retained-screen"))
      )
      val month = YearMonth.of(2026, 10)
      def report(value: InvestmentScreen, captureMonth: YearMonth): InvestmentEvaluation =
        InvestmentEvaluator.evaluate(value.id, value.createdAt, now, value.cohorts, Map.empty, captureMonth, captureMonth.atEndOfMonth())
      val financial = FinancialSnapshot(AAPL, Provider.Finnhub, now, Map.empty, Nil, Nil, "hash", "mapping", Nil)
      val date = LocalDate.of(2026, 10, 16)
      def history(ticker: Ticker, adjustment: PriceAdjustment): PriceHistorySnapshot =
        PriceHistorySnapshot(
          s"provider-${ticker.value}-${adjustment.value}", ticker, List(PriceCandle(date, 100, 100, 100, 100, 1L)),
          Provider.TwelveData, "USD", adjustment, now, DateRange(date, date)
        )
      val purgeId = JobId(s"purge-${AAPL.value}-$now")
      withEmbeddedMongoDatabase(seed) { db =>
        for
          repos <- Repositories.make[IO](db)
          dispatcher <- ActionDispatcher.make[IO](repos.jobs)
          services <- Services.make[IO](clients, repos, dispatcher, InvestmentConfig(7, 14, 180, 550), SecurityConfig(0.2))
          handler <- ActionHandler.make[IO](services)
          _ <- List(AAPL, MSFT).traverse_(ticker => PriceAdjustment.values.toList.traverse_(adjustment => repos.priceHistory.save(history(ticker, adjustment))))
          _ <- List(financial, financial.copy(ticker = MSFT)).traverse_(repos.financialHistory.save)
          _ <- repos.investmentRankings.save(ranking, entries ++ retainedEntries)
          _ <- repos.investmentRankings.save(retainedRanking, retainedEntries)
          _ <- List(affected, retainedScreen).traverse_(repos.investmentScreens.save)
          _ <- List(report(affected, month), report(affected, month.minusMonths(1)), report(retainedScreen, month)).traverse_(repos.investmentEvaluations.saveMonthly)
          _ <- List(ranking.id, retainedRanking.id).traverse_(id => run(handler, Action.PinInvestmentRanking(id)))
          _ <- run(handler, Action.SetSecurityActive(AAPL, false, now, "user-excluded"))
          job <- dispatcher.findJob(purgeId).flatMap(IO.fromOption(_)(new IllegalStateException("Missing durable purge job")))
          _ <- run(handler, job.action)
          _ <- run(handler, Action.SetSecurityActive(AAPL, false, now, "user-excluded"))
          _ <- run(handler, job.action)
          tombstone <- readSecurity(db, AAPL)
          securities <- ids(db, SecurityRepository.CollectionName)
          profiles <- ids(db, CompanyProfileRepository.CollectionName)
          analytics <- ids(db, PriceAnalyticsRepository.CollectionName)
          prices <- ids(db, PriceHistoryRepository.CollectionName)
          metrics <- ids(db, FinancialMetricsRepository.CollectionName)
          financials <- ids(db, FinancialHistoryRepository.CollectionName)
          screens <- ids(db, InvestmentScreenRepository.CollectionName)
          evaluations <- ids(db, InvestmentEvaluationRepository.CollectionName)
          inputs <- storedRankingEntries(db, ranking.id)
          retainedInputs <- storedRankingEntries(db, retainedRanking.id)
          headers <- db.getCollection(InvestmentRankingRepository.CollectionName)
          released <- headers.find(Filter.idEq(ranking.id.value)).first
          pinned <- headers.find(Filter.idEq(retainedRanking.id.value)).first
          jobs <- ids(db, JobRepository.CollectionName)
        yield
          job.action mustBe Action.PurgeStock(AAPL, now)
          tombstone.map(_.isActive) mustBe Some(false)
          tombstone.flatMap(_.exclusionReason) mustBe Some("user-excluded")
          tombstone.flatMap(_.companyProfileLastUpdatedAt) mustBe None
          securities mustBe Set(AAPL.value, MSFT.value)
          List(profiles, analytics, metrics, financials).foreach(_ mustBe Set(MSFT.value))
          prices mustBe PriceAdjustment.values.map(adjustment => s"${MSFT.value}:${adjustment.value}").toSet
          screens mustBe Set(retainedScreen.id.value)
          evaluations mustBe Set(s"${retainedScreen.id.value}:$month")
          inputs mustBe retainedEntries
          retainedInputs mustBe retainedEntries
          released.flatMap(_.get("expiresAt")).flatMap(_.asInstant) mustBe Some(now.plusSeconds(3L * 86400))
          pinned.exists(_.keys.contains("expiresAt")) mustBe false
          jobs mustBe Set(purgeId.value)
      }
    }
  }
