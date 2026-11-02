package stockschecker.repositories

import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import stockschecker.domain.{InvestmentRecipe, Ticker}
import stockschecker.repositories.queries.PriceAnalyticsReadinessQuery

import java.time.{Instant, LocalDate}

class PriceAnalyticsReadinessQuerySpec extends RepositorySpec:
  override def port: Int = 12174
  private val now        = Instant.parse("2026-11-02T12:00:00Z")

  "The price analytics readiness query" should {
    "project persisted price readiness without decoding unrelated analytics fields" in {
      val seed = Map(
        PriceAnalyticsRepository.CollectionName -> List(
          Document(
            "_id"                := "ZZZ",
            "updatedAt"          := now,
            "performanceSummary" := Document("latestPrice" := BigDecimal(100), "latestPriceDate" := "2026-10-01"),
            "metrics"            := "not needed by this query"
          ),
          Document(
            "_id"                := "AAA",
            "updatedAt"          := now.minusSeconds(60),
            "performanceSummary" := Document("latestPrice" := BigDecimal(150), "latestPriceDate" := "2026-11-02")
          )
        )
      )
      withEmbeddedMongoDatabase(seed) { db =>
        for
          collection <- db.getCollectionWithCodec[PriceAnalyticsReadinessQuery.Row](PriceAnalyticsRepository.CollectionName)
          rows       <- collection.aggregate[PriceAnalyticsReadinessQuery.Row](PriceAnalyticsReadinessQuery.pipeline).all
        yield
          rows.map(_.ticker) mustBe List(Ticker("AAA"), Ticker("ZZZ"))
          val ready = rows.head.toDomain(now, InvestmentRecipe())
          ready.usable mustBe true
          ready.calculatedAt mustBe Some(now.minusSeconds(60))
          ready.latestPriceDate mustBe Some(LocalDate.parse("2026-11-02"))
          ready.latestPrice mustBe Some(BigDecimal(150))
          rows.last.toDomain(now, InvestmentRecipe()).reason mustBe Some("stale-price")
      }
    }
  }
