package stockschecker.repositories

import cats.effect.IO
import cats.syntax.parallel.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.operations.Filter
import stockschecker.domain.{FinancialMetric, FinancialObservation, FinancialSnapshot, FinancialSnapshotIssue, Provider}
import stockschecker.fixtures.*
import stockschecker.repositories.entities.FinancialSnapshotEntity

import java.time.{Instant, LocalDate}

class FinancialHistoryRepositorySpec extends RepositorySpec with MongoJsonCodecs:
  override def port: Int = 12159
  private val first      = FinancialSnapshot(
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
          first.copy(observedAt = ts.plusSeconds(60), metrics = Map(FinancialMetric.PriceToEarningsTtm -> BigDecimal(30)))
        val other = first.copy(ticker = MSFT)
        for
          repo       <- FinancialHistoryRepository.make[IO](db)
          _          <- repo.save(first)
          _          <- repo.save(second)
          _          <- repo.save(other)
          collection <- db.getCollectionWithCodec[FinancialSnapshotEntity](FinancialHistoryRepository.CollectionName)
          retained   <- collection.find.all
          raw        <- db.getCollection(FinancialHistoryRepository.CollectionName)
          documents  <- raw.find.all
        yield
          retained.map(_.toDomain).toSet mustBe Set(second, other)
          documents.map(_.get("_id").flatMap(_.asString)).toSet mustBe Set(Some(AAPL.value), Some(MSFT.value))
          documents.forall(_.get("id").isEmpty) mustBe true
          documents.forall(_.get("rawPayload").isEmpty) mustBe true
      }

    "keep the newest immutable snapshot after duplicate, out-of-order and tied-timestamp saves" in
      withEmbeddedMongoDatabase { db =>
        val newest = first.copy(observedAt = ts.plusSeconds(60), payloadHash = "newest-hash")
        val tied   = newest.copy(payloadHash = "tied-hash")
        for
          repo       <- FinancialHistoryRepository.make[IO](db)
          _          <- repo.save(newest)
          _          <- repo.save(newest)
          _          <- repo.save(newest.copy(metrics = Map.empty))
          _          <- repo.save(first)
          _          <- repo.save(tied)
          collection <- db.getCollectionWithCodec[FinancialSnapshotEntity](FinancialHistoryRepository.CollectionName)
          retained   <- collection.find.all
        yield retained.map(_.toDomain).toList mustBe List(newest)
      }

    "retain the newest snapshot when refreshes finish concurrently" in
      withEmbeddedMongoDatabase { db =>
        val snapshots = (0 to 4).toList.map(index => first.copy(observedAt = ts.plusSeconds(index.toLong), payloadHash = s"hash-$index"))
        for
          repo       <- FinancialHistoryRepository.make[IO](db)
          _          <- snapshots.parTraverse_(repo.save)
          collection <- db.getCollectionWithCodec[FinancialSnapshotEntity](FinancialHistoryRepository.CollectionName)
          retained   <- collection.find.all
        yield retained.map(_.toDomain).toList mustBe List(snapshots.last)
      }

    "persist BSON dates at millisecond precision and reject timestamps in the same millisecond" in
      withEmbeddedMongoDatabase { db =>
        val second = Instant.parse("2026-10-29T12:00:00Z")
        val whole  = first.copy(observedAt = second, payloadHash = "whole-hash")
        val millis = first.copy(observedAt = second.plusMillis(1), payloadHash = "millis-hash")
        val nanos  = first.copy(observedAt = second.plusMillis(1).plusNanos(1), payloadHash = "nanos-hash")
        for
          repo       <- FinancialHistoryRepository.make[IO](db)
          _          <- repo.save(whole)
          _          <- repo.save(millis)
          _          <- repo.save(nanos)
          collection <- db.getCollectionWithCodec[FinancialSnapshotEntity](FinancialHistoryRepository.CollectionName)
          retained   <- collection.find.all
          raw        <- db.getCollection(FinancialHistoryRepository.CollectionName)
          dates      <- raw.count(Filter.idEq(AAPL.value) && Filter.typeIs("observedAt", "date"))
        yield
          retained.map(_.toDomain).toList mustBe List(millis)
          dates mustBe 1L
      }
  }
