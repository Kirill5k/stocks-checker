package stockschecker.repositories

import cats.effect.IO
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.operations.Filter
import stockschecker.domain.*
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
    "preserve immutable vintages and find only histories covering the requested date range" in
      withEmbeddedMongoDatabase { db =>
        for
          repo    <- PriceHistoryRepository.make[IO](db)
          _       <- repo.save(original)
          _       <- repo.save(original.copy(currency = "EUR"))
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

    "select an older fully covering snapshot when newer snapshots omit either end of the requested range" in
      withEmbeddedMongoDatabase { db =>
        val laterStart = original.copy(
          id = "later-start",
          fetchedAt = original.fetchedAt.plusSeconds(1),
          requestedRange = DateRange(from.plusDays(1), through)
        )
        val earlierEnd = original.copy(
          id = "earlier-end",
          fetchedAt = original.fetchedAt.plusSeconds(2),
          requestedRange = DateRange(from, through.minusDays(1))
        )
        for
          repo    <- PriceHistoryRepository.make[IO](db)
          _       <- List(original, laterStart, earlierEnd).traverse_(repo.save)
          latest  <- repo.latest(ticker, PriceAdjustment.SplitsAndDividends)
          covered <- repo.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, DateRange(from, through))
        yield
          latest mustBe Some(earlierEnd)
          covered mustBe Some(original)
      }

    "bound retained vintages without evicting broader history needed by other screens" in
      withEmbeddedMongoDatabase { db =>
        val broad   = original.copy(id = "broad", requestedRange = DateRange(from.minusYears(13), through))
        val updates = (1 to 6).toList.map(i => original.copy(id = s"short-$i", fetchedAt = original.fetchedAt.plusSeconds(i.toLong)))
        val raw     = original.copy(id = "raw", adjustment = PriceAdjustment.Unadjusted)
        val split   = original.copy(id = "split", adjustment = PriceAdjustment.Splits)
        val other   = original.copy(id = "other", ticker = Ticker("BBB"))
        for
          repo          <- PriceHistoryRepository.make[IO](db)
          _             <- repo.save(broad)
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
          covered       <- repo.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, DateRange(from.minusYears(10), through))
        yield
          retained.size mustBe 4
          rawRetained mustBe Some(raw)
          splitRetained mustBe Some(split)
          otherRetained mustBe Some(other)
          latest mustBe updates.lastOption
          covered mustBe Some(broad)
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
              "_id"        := id,
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
                  Filter.idEq(saved.id) && Filter.eq("adjustment", label) && Filter.eq("data.adjustment", label) &&
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
