package stockschecker.repositories

import stockschecker.domain.Provider
import cats.effect.IO
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.operations.Filter
import stockschecker.domain.{FinancialMetric, FinancialSnapshot}
import stockschecker.fixtures.*
import stockschecker.repositories.entities.FinancialHistoryEntity
import io.circe.syntax.*

class FinancialHistoryRepositorySpec extends RepositorySpec with MongoJsonCodecs:
  override def port: Int = 12159

  "FinancialHistoryRepository" should {
    "retain older immutable vintages and return the latest per ticker" in
      withEmbeddedMongoDatabase { db =>
        val first = FinancialSnapshot(
          "first",
          AAPL,
          Provider.Finnhub,
          ts,
          Map(FinancialMetric.PriceToEarningsTtm -> BigDecimal(20)),
          Nil,
          Nil,
          "first-hash",
          "canonical-test-v1",
          Nil
        )
        val second =
          first.copy(id = "second", observedAt = ts.plusSeconds(60), metrics = Map(FinancialMetric.PriceToEarningsTtm -> BigDecimal(30)))
        val other = first.copy(id = "other", ticker = MSFT)
        for
          repo       <- FinancialHistoryRepository.make[IO](db)
          _          <- repo.save(first, "large-raw-payload")
          _          <- repo.save(second, "second-payload")
          _          <- repo.save(other, "other-payload")
          latest     <- repo.findLatest(AAPL)
          allLatest  <- repo.findLatestAll
          collection <- db.getCollectionWithCodec[FinancialSnapshot](FinancialHistoryRepository.CollectionName)
          all        <- collection.find(Filter.empty).all
        yield
          latest mustBe Some(second)
          allLatest.map(_.id).toSet mustBe Set("second", "other")
          all.map(_.id).toSet mustBe Set("first", "second", "other")
          all.find(_.id == "first") mustBe Some(first)
      }
    "bound raw vintages per ticker and omit payloads from universe reads" in
      withEmbeddedMongoDatabase { db =>
        val first = FinancialSnapshot(
          "first",
          AAPL,
          Provider.Finnhub,
          ts,
          Map.empty,
          Nil,
          Nil,
          "hash",
          "canonical-test-v1",
          Nil
        )
        for
          repo       <- FinancialHistoryRepository.make[IO](db)
          _          <- repo.save(first, "large-raw-payload")
          _          <- repo.save(first.copy(id = "second", observedAt = ts.plusSeconds(1)), "large-raw-payload")
          _          <- repo.save(first.copy(id = "third", observedAt = ts.plusSeconds(2)), "large-raw-payload")
          _          <- repo.save(first.copy(id = "fourth", observedAt = ts.plusSeconds(3)), "large-raw-payload")
          _          <- repo.save(first.copy(id = "fourth", observedAt = ts.plusSeconds(3)), "overwrite-attempt")
          collection <- db.getCollectionWithCodec[FinancialHistoryEntity](FinancialHistoryRepository.CollectionName)
          retained   <- collection.find.all
          latest     <- repo.findLatestAll
        yield
          retained.map(_.snapshot.id).toSet mustBe Set("second", "third", "fourth")
          retained.find(_.snapshot.id == "fourth").map(_.rawPayload) mustBe Some("large-raw-payload")
          latest.map(_.id) mustBe List("fourth")
          latest.head.asJson.hcursor.downField("rawPayload").succeeded mustBe false
      }
  }
