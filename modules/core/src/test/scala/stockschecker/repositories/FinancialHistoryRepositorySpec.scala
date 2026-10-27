package stockschecker.repositories

import stockschecker.domain.Provider
import cats.effect.IO
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.operations.Filter
import stockschecker.domain.FinancialSnapshot
import stockschecker.fixtures.*

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
          Map("peTTM" -> BigDecimal(20)),
          Nil,
          Nil,
          "first-payload",
          "first-hash",
          FinancialSnapshot.FinnhubMappingVersion,
          Nil
        )
        val second = first.copy(id = "second", observedAt = ts.plusSeconds(60), metrics = Map("peTTM" -> BigDecimal(30)))
        val other  = first.copy(id = "other", ticker = MSFT)
        for
          repo       <- FinancialHistoryRepository.make[IO](db)
          _          <- repo.save(first)
          _          <- repo.save(second)
          _          <- repo.save(other)
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
          "large-raw-payload",
          "hash",
          FinancialSnapshot.FinnhubMappingVersion,
          Nil
        )
        for
          repo       <- FinancialHistoryRepository.make[IO](db)
          _          <- repo.save(first)
          _          <- repo.save(first.copy(id = "second", observedAt = ts.plusSeconds(1)))
          _          <- repo.save(first.copy(id = "third", observedAt = ts.plusSeconds(2)))
          _          <- repo.save(first.copy(id = "fourth", observedAt = ts.plusSeconds(3)))
          _          <- repo.save(first.copy(id = "fourth", observedAt = ts.plusSeconds(3), rawPayload = "overwrite-attempt"))
          collection <- db.getCollectionWithCodec[FinancialSnapshot](FinancialHistoryRepository.CollectionName)
          retained   <- collection.find.all
          latest     <- repo.findLatestAll
        yield
          retained.map(_.id).toSet mustBe Set("second", "third", "fourth")
          retained.find(_.id == "fourth").map(_.rawPayload) mustBe Some("large-raw-payload")
          latest.map(_.id) mustBe List("fourth")
          latest.map(_.rawPayload) mustBe List("")
      }
  }
