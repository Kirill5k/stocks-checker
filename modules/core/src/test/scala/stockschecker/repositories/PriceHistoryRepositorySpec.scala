package stockschecker.repositories

import cats.effect.IO
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.operations.Filter
import stockschecker.domain.*
import stockschecker.repositories.entities.PriceHistoryEntity
import java.time.{Instant, LocalDate}

class PriceHistoryRepositorySpec extends RepositorySpec {
  override def port: Int = 12163
  private val ticker     = Ticker("AAA")
  private val from       = LocalDate.parse("2025-01-01")
  private val through    = LocalDate.parse("2025-02-01")
  private val bar        = PriceCandle(from.plusDays(1), 100, 100, 100, 100, 1L)
  private val original   = PriceHistorySnapshot(
    "price-vintage-1",
    ticker,
    List(bar),
    Provider.TwelveData,
    "USD",
    PriceAdjustment.SplitsAndDividends,
    Instant.parse("2025-02-02T12:00:00Z"),
    DateRange(from, through)
  )

  "Price history repository" should {
    "keep retries of the current snapshot immutable and find only histories covering the requested date range" in
      withEmbeddedMongoDatabase { db =>
        for
          repo    <- PriceHistoryRepository.make[IO](db)
          _       <- repo.save(original)
          _       <- repo.save(original.copy(currency = "EUR"))
          _       <- repo.save(original.copy(currency = "EUR", fetchedAt = original.fetchedAt.plusSeconds(1)))
          latest  <- repo.latest(ticker, PriceAdjustment.SplitsAndDividends)
          covered <- repo.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, DateRange(from, through))
          before  <- repo.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, DateRange(from.minusDays(1), through))
          after   <- repo.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, DateRange(from, through.plusDays(1)))
        yield {
          latest mustBe Some(original)
          covered mustBe Some(original)
          before mustBe None
          after mustBe None
        }
      }

    "replace the current snapshot in place without merging or retaining older candles" in
      withEmbeddedMongoDatabase { db =>
        val replacement = original.copy(
          id = "replacement-vintage",
          fetchedAt = original.fetchedAt.plusNanos(1),
          candles = List(bar.copy(date = bar.date.plusDays(1), close = 200)),
          requestedRange = DateRange(from.plusDays(1), through.minusDays(1))
        )
        for
          repo       <- PriceHistoryRepository.make[IO](db)
          _          <- repo.save(original)
          _          <- repo.save(replacement)
          latest     <- repo.latest(ticker, PriceAdjustment.SplitsAndDividends)
          covered    <- repo.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, replacement.requestedRange)
          oldRange   <- repo.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, original.requestedRange)
          collection <- db.getCollectionWithCodec[PriceHistoryEntity](PriceHistoryRepository.CollectionName)
          retained   <- collection.find.all
        yield
          latest mustBe Some(replacement)
          covered mustBe Some(replacement)
          oldRange mustBe None
          retained.toList mustBe List(PriceHistoryEntity("AAA:split-and-dividend", ticker, original.adjustment, replacement))
      }

    "keep one independent cache slot per ticker and adjustment" in
      withEmbeddedMongoDatabase { db =>
        val updates = (1 to 6).toList.map(i => original.copy(id = s"short-$i", fetchedAt = original.fetchedAt.plusSeconds(i.toLong)))
        val raw     = original.copy(id = "raw", adjustment = PriceAdjustment.Unadjusted)
        val split   = original.copy(id = "split", adjustment = PriceAdjustment.Splits)
        val other   = original.copy(id = "other", ticker = Ticker("BBB"))
        for
          repo          <- PriceHistoryRepository.make[IO](db)
          _             <- repo.save(original)
          _             <- repo.save(raw)
          _             <- repo.save(split)
          _             <- repo.save(other)
          _             <- updates.traverse_(repo.save)
          collection    <- db.getCollection("price-observations")
          retained      <- collection.find(Filter.eq("ticker", ticker.value) && Filter.eq("adjustment", "split-and-dividend")).all
          rawRetained   <- repo.latest(ticker, PriceAdjustment.Unadjusted)
          splitRetained <- repo.latest(ticker, PriceAdjustment.Splits)
          otherRetained <- repo.latest(other.ticker, PriceAdjustment.SplitsAndDividends)
          latest        <- repo.latest(ticker, PriceAdjustment.SplitsAndDividends)
          all           <- collection.find.all
        yield
          retained.size mustBe 1
          all.size mustBe 4
          rawRetained mustBe Some(raw)
          splitRetained mustBe Some(split)
          otherRetained mustBe Some(other)
          latest mustBe updates.lastOption
      }

    "keep the newest snapshot across concurrent, out-of-order and tied-timestamp saves" in
      withEmbeddedMongoDatabase { db =>
        val snapshots = (0 to 8).toList.map(i => original.copy(id = s"vintage-$i", fetchedAt = original.fetchedAt.plusNanos(i.toLong)))
        val newest    = snapshots.last
        for
          repo       <- PriceHistoryRepository.make[IO](db)
          _          <- snapshots.reverse.parTraverse_(repo.save)
          _          <- repo.save(original)
          _          <- repo.save(newest.copy(id = "tied-vintage", currency = "EUR"))
          _          <- repo.save(newest.copy(currency = "EUR", fetchedAt = newest.fetchedAt.plusSeconds(1)))
          collection <- db.getCollectionWithCodec[PriceHistoryEntity](PriceHistoryRepository.CollectionName)
          retained   <- collection.find.all
          latest     <- repo.latest(ticker, original.adjustment)
        yield
          latest mustBe Some(newest)
          retained.toList mustBe List(PriceHistoryEntity("AAA:split-and-dividend", ticker, original.adjustment, newest))
      }

    "round-trip adjustment labels and nested requested ranges in BSON" in
      withEmbeddedMongoDatabase { db =>
        val adjustments = List(
          PriceAdjustment.Unadjusted         -> "none",
          PriceAdjustment.Splits             -> "splits",
          PriceAdjustment.SplitsAndDividends -> "split-and-dividend"
        )
        for
          collection <- db.getCollection("price-observations")
          _          <- collection.insertMany(adjustments.map { case (_, label) =>
            val id = s"seeded-$label"
            Document(
              "_id"        := s"${ticker.value}:$label",
              "ticker"     := ticker.value,
              "adjustment" := label,
              "data"       := Document(
                "id"      := id,
                "ticker"  := ticker.value,
                "candles" := List(
                  Document(
                    "date"   := bar.date.toString,
                    "open"   := 100,
                    "high"   := 100,
                    "low"    := 100,
                    "close"  := 100,
                    "volume" := 1L
                  )
                ),
                "provider"       := "TwelveData",
                "currency"       := "USD",
                "adjustment"     := label,
                "fetchedAt"      := original.fetchedAt.toString,
                "requestedRange" := Document("from" := from.toString, "through" := through.toString)
              )
            )
          })
          repo    <- PriceHistoryRepository.make[IO](db)
          results <- adjustments.traverse { case (adjustment, label) =>
            val seeded = original.copy(id = s"seeded-$label", adjustment = adjustment)
            val saved  = seeded.copy(id = s"enum-$label", fetchedAt = original.fetchedAt.plusSeconds(1))
            for
              existing <- repo.latest(ticker, adjustment)
              covering <- repo.latestCovering(ticker, adjustment, DateRange(from, through))
              _        <- repo.save(saved)
              latest   <- repo.latest(ticker, adjustment)
              raw      <- collection
                .find(
                  Filter.idEq(s"${ticker.value}:$label") && Filter.eq("data.id", saved.id) &&
                    Filter.eq("adjustment", label) && Filter.eq("data.adjustment", label) &&
                    Filter.eq("data.requestedRange.from", from.toString) && Filter.eq("data.requestedRange.through", through.toString)
                )
                .first
            yield (seeded, saved, existing, covering, latest, raw.isDefined)
          }
        yield
          results.foreach { case (seeded, saved, existing, covering, latest, matchesSchema) =>
            existing mustBe Some(seeded)
            covering mustBe Some(seeded)
            latest mustBe Some(saved)
            matchesSchema mustBe true
          }
          succeed
      }
  }
}
