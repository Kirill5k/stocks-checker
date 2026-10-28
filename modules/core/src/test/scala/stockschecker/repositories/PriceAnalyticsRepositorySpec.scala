package stockschecker.repositories

import cats.data.NonEmptyList
import stockschecker.domain.PriceAnalyticsCalculator
import cats.effect.IO
import mongo4cats.bson.BsonValue
import stockschecker.domain.{PriceAnalytics, PriceAnalyticsFilter, PriceCandle}
import stockschecker.fixtures.*

import java.time.LocalDate

class PriceAnalyticsRepositorySpec extends RepositorySpec {

  override def port: Int = 12152

  val MSFTPriceCandles: NonEmptyList[PriceCandle] = NonEmptyList.of(
    PriceCandle(
      date = LocalDate.parse("2025-10-01"),
      open = BigDecimal("375.00"),
      high = BigDecimal("385.00"),
      low = BigDecimal("370.00"),
      close = BigDecimal("380.00"),
      volume = 30000000L
    ),
    PriceCandle(
      date = LocalDate.parse("2025-09-01"),
      open = BigDecimal("370.00"),
      high = BigDecimal("378.00"),
      low = BigDecimal("365.00"),
      close = BigDecimal("375.00"),
      volume = 28000000L
    ),
    PriceCandle(
      date = LocalDate.parse("2025-08-01"),
      open = BigDecimal("360.00"),
      high = BigDecimal("372.00"),
      low = BigDecimal("355.00"),
      close = BigDecimal("368.00"),
      volume = 32000000L
    )
  )

  val MSFTPriceAnalytics: PriceAnalytics = PriceAnalyticsCalculator.from(MSFT, MSFTPriceCandles)

  "A PriceAnalyticsRepository" when {
    "save" should {
      "save price analytics in the repository" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- PriceAnalyticsRepository.make[IO](db)
            _    <- repo.save(AAPLPriceAnalytics)
            res  <- repo.find(AAPL)
          yield res mustBe Some(AAPLPriceAnalytics)
        }

      "update analytics when saving same ticker" in
        withEmbeddedMongoDatabase { db =>
          val updatedAnalytics = AAPLPriceAnalytics.copy(
            performanceSummary = AAPLPriceAnalytics.performanceSummary.copy(latestPrice = BigDecimal("235.00"))
          )
          for
            repo <- PriceAnalyticsRepository.make[IO](db)
            _    <- repo.save(AAPLPriceAnalytics)
            _    <- repo.save(updatedAnalytics)
            res  <- repo.find(AAPL)
          yield res.map(_.performanceSummary.latestPrice) mustBe Some(BigDecimal("235.00"))
        }

      "preserve absent basis and allow refreshed basis to be written or cleared" in
        withEmbeddedMongoDatabase { db =>
          val withoutBasis = AAPLPriceAnalytics.copy(basis = None)
          for
            repo      <- PriceAnalyticsRepository.make[IO](db)
            _         <- repo.save(withoutBasis)
            absent    <- repo.find(AAPL)
            _         <- repo.save(AAPLPriceAnalytics)
            refreshed <- repo.find(AAPL)
            _         <- repo.save(withoutBasis)
            cleared   <- repo.find(AAPL)
          yield
            absent mustBe Some(withoutBasis)
            refreshed mustBe Some(AAPLPriceAnalytics)
            refreshed.flatMap(_.basis) mustBe defined
            cleared mustBe Some(withoutBasis)
        }

      "save batch of analytics" in
        withEmbeddedMongoDatabase { db =>
          val withoutBasis  = AAPLPriceAnalytics.copy(basis = None)
          val analyticsList = List(withoutBasis, MSFTPriceAnalytics)
          for
            repo <- PriceAnalyticsRepository.make[IO](db)
            _    <- repo.save(analyticsList)
            res1 <- repo.find(AAPL)
            res2 <- repo.find(MSFT)
          yield
            res1 mustBe Some(withoutBasis)
            res2 mustBe Some(MSFTPriceAnalytics)
        }
    }

    "find" should {
      "preserve analytics values when basis is missing or explicitly null" in {
        val seedData = Map(
          PriceAnalyticsRepository.CollectionName -> List(
            toDoc(AAPLPriceAnalytics).remove("basis"),
            toDoc(MSFTPriceAnalytics).add("basis", BsonValue.Null)
          )
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            repo <- PriceAnalyticsRepository.make[IO](db)
            aapl <- repo.find(AAPL)
            msft <- repo.find(MSFT)
            all  <- repo.findAll(None)
          yield
            aapl mustBe Some(AAPLPriceAnalytics.copy(basis = None))
            msft mustBe Some(MSFTPriceAnalytics.copy(basis = None))
            all.toSet mustBe Set(AAPLPriceAnalytics.copy(basis = None), MSFTPriceAnalytics.copy(basis = None))
        }
      }

      "return None when analytics does not exist for ticker" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- PriceAnalyticsRepository.make[IO](db)
            res  <- repo.find(MSFT)
          yield res mustBe None
        }

      "find analytics by ticker" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- PriceAnalyticsRepository.make[IO](db)
            _    <- repo.save(AAPLPriceAnalytics)
            res  <- repo.find(AAPL)
          yield
            res mustBe defined
            res.map(_.ticker) mustBe Some(AAPL)
            res.map(_.performanceSummary.latestPrice) mustBe Some(BigDecimal("228.50"))
        }
    }

    "findAll" should {
      "return empty list when no analytics exist" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- PriceAnalyticsRepository.make[IO](db)
            res  <- repo.findAll(None)
          yield res mustBe List.empty
        }

      "return all analytics sorted by overall score" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- PriceAnalyticsRepository.make[IO](db)
            _    <- repo.save(List(AAPLPriceAnalytics, MSFTPriceAnalytics))
            res  <- repo.findAll(None)
          yield
            res.length mustBe 2
            res.head.ticker mustBe (if (AAPLPriceAnalytics.scores.overallScore >= MSFTPriceAnalytics.scores.overallScore) AAPL else MSFT)
        }

      "respect limit parameter" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- PriceAnalyticsRepository.make[IO](db)
            _    <- repo.save(List(AAPLPriceAnalytics, MSFTPriceAnalytics))
            res  <- repo.findAll(Some(1))
          yield res.length mustBe 1
        }
    }

    "findBy" should {
      "filter by price above" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- PriceAnalyticsRepository.make[IO](db)
            _    <- repo.save(List(AAPLPriceAnalytics, MSFTPriceAnalytics))
            res  <- repo.findBy(PriceAnalyticsFilter.PriceAbove(BigDecimal("300")), None)
          yield
            res.length mustBe 1
            res.head.ticker mustBe MSFT
        }

      "filter by price below" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- PriceAnalyticsRepository.make[IO](db)
            _    <- repo.save(List(AAPLPriceAnalytics, MSFTPriceAnalytics))
            res  <- repo.findBy(PriceAnalyticsFilter.PriceBelow(BigDecimal("300")), None)
          yield
            res.length mustBe 1
            res.head.ticker mustBe AAPL
        }

      "filter by composite filters" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- PriceAnalyticsRepository.make[IO](db)
            _    <- repo.save(List(AAPLPriceAnalytics, MSFTPriceAnalytics))
            filter = PriceAnalyticsFilter.Composite(
              NonEmptyList.of(
                PriceAnalyticsFilter.PriceAbove(BigDecimal("200")),
                PriceAnalyticsFilter.PriceBelow(BigDecimal("300"))
              )
            )
            res <- repo.findBy(filter, None)
          yield
            res.length mustBe 1
            res.head.ticker mustBe AAPL
        }
    }
  }
}
