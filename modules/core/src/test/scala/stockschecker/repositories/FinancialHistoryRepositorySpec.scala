package stockschecker.repositories

import cats.effect.IO
import cats.syntax.parallel.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.operations.Filter
import stockschecker.domain.{FinancialMetric, FinancialObservation, FinancialSnapshot, FinancialSnapshotIssue, Provider}
import stockschecker.fixtures.*

import java.time.{Instant, LocalDate}

class FinancialHistoryRepositorySpec extends RepositorySpec with MongoJsonCodecs:
  override def port: Int = 12159
  private val first      = FinancialSnapshot(
    "first",
    AAPL,
    Provider.Finnhub,
    ts,
    Map(FinancialMetric.PriceToEarningsTtm -> BigDecimal(20)),
    List(FinancialObservation(FinancialMetric.ReturnOnAssets, LocalDate.parse("2024-12-31"), BigDecimal("0.2"))),
    List(FinancialObservation(FinancialMetric.FreeCashFlowPerShareTtm, LocalDate.parse("2025-09-30"), BigDecimal("1.5"))),
    "first-hash",
    "canonical-test-v1",
    List(FinancialSnapshotIssue.SymbolMismatch)
  )

  "FinancialHistoryRepository" should {
    "retain only the latest canonical snapshot per ticker, preserving observations and provenance without raw payloads" in
      withEmbeddedMongoDatabase { db =>
        val second =
          first.copy(id = "second", observedAt = ts.plusSeconds(60), metrics = Map(FinancialMetric.PriceToEarningsTtm -> BigDecimal(30)))
        val other = first.copy(id = "other", ticker = MSFT)
        for
          repo       <- FinancialHistoryRepository.make[IO](db)
          _          <- repo.save(first)
          _          <- repo.save(second)
          _          <- repo.save(other)
          latest     <- repo.findLatest(AAPL)
          allLatest  <- repo.findLatestAll
          collection <- db.getCollectionWithCodec[FinancialSnapshot](FinancialHistoryRepository.CollectionName)
          retained   <- collection.find.all
          raw        <- db.getCollection(FinancialHistoryRepository.CollectionName)
          payloads   <- raw.find(Filter.ne("rawPayload", Option.empty[String])).all
        yield
          latest mustBe Some(second)
          allLatest mustBe List(second, other)
          retained.toSet mustBe Set(second, other)
          payloads mustBe empty
      }

    "keep the newest immutable snapshot after duplicate, out-of-order and tied-timestamp saves" in
      withEmbeddedMongoDatabase { db =>
        val newest = first.copy(id = "z-newest", observedAt = ts.plusSeconds(60))
        val tied   = newest.copy(id = "a-tied")
        for
          repo       <- FinancialHistoryRepository.make[IO](db)
          _          <- repo.save(newest)
          _          <- repo.save(newest.copy(metrics = Map.empty))
          _          <- repo.save(first)
          _          <- repo.save(tied)
          latest     <- repo.findLatest(AAPL)
          allLatest  <- repo.findLatestAll
          collection <- db.getCollectionWithCodec[FinancialSnapshot](FinancialHistoryRepository.CollectionName)
          retained   <- collection.find.all
        yield
          latest mustBe Some(newest)
          allLatest mustBe List(newest)
          retained.toList mustBe List(newest)
      }

    "retain the newest snapshot when refreshes finish concurrently" in
      withEmbeddedMongoDatabase { db =>
        val snapshots = (0 to 4).toList.map(index => first.copy(id = s"snapshot-$index", observedAt = ts.plusSeconds(index.toLong)))
        for
          repo       <- FinancialHistoryRepository.make[IO](db)
          _          <- snapshots.parTraverse_(repo.save)
          collection <- db.getCollectionWithCodec[FinancialSnapshot](FinancialHistoryRepository.CollectionName)
          retained   <- collection.find.all
        yield retained.toList mustBe List(snapshots.last)
      }

    "compare whole-second, millisecond and nanosecond timestamps chronologically when reading and pruning existing snapshots" in
      withEmbeddedMongoDatabase { db =>
        val second = Instant.parse("2026-10-29T12:00:00Z")
        val whole  = first.copy(id = "z-whole", observedAt = second)
        val millis = first.copy(id = "z-millis", observedAt = second.plusMillis(1))
        val nanos  = first.copy(id = "a-nanos", observedAt = second.plusMillis(1).plusNanos(1))
        for
          repo       <- FinancialHistoryRepository.make[IO](db)
          collection <- db.getCollectionWithCodec[FinancialSnapshot](FinancialHistoryRepository.CollectionName)
          _          <- collection.insertMany(List(whole, millis, nanos))
          latest     <- repo.findLatest(AAPL)
          allLatest  <- repo.findLatestAll
          _          <- repo.save(whole)
          retained   <- collection.find.all
        yield
          latest mustBe Some(nanos)
          allLatest mustBe List(nanos)
          retained.toList mustBe List(nanos)
      }
  }
