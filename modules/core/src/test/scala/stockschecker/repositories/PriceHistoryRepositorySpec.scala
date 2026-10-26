package stockschecker.repositories

import cats.effect.IO
import cats.syntax.all.*
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
    "TwelveData",
    "USD",
    PriceAdjustment.SplitsAndDividends,
    Instant.parse("2025-02-02T12:00:00Z"),
    from,
    through
  )

  "Price history repository" should {
    "preserve immutable vintages and find only histories covering the requested starting date" in
      withEmbeddedMongoDatabase { db =>
        for
          repo    <- PriceHistoryRepository.make[IO](db)
          _       <- repo.save(original)
          _       <- repo.save(original.copy(currency = "EUR"))
          latest  <- repo.latest(ticker, PriceAdjustment.SplitsAndDividends)
          covered <- repo.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, from.plusDays(1))
          before  <- repo.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, from.minusDays(1))
        yield {
          latest mustBe Some(original)
          covered mustBe Some(original)
          before mustBe None
        }
      }
    "bound retained vintages without evicting broader history needed by other screens" in
      withEmbeddedMongoDatabase { db =>
        val broad   = original.copy(id = "broad", requestedFrom = from.minusYears(13))
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
          covered       <- repo.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, from.minusYears(10))
        yield
          retained.size mustBe 4
          rawRetained mustBe Some(raw)
          splitRetained mustBe Some(split)
          otherRetained mustBe Some(other)
          latest mustBe updates.lastOption
          covered mustBe Some(broad)
      }

    "read legacy adjustment strings and preserve their BSON labels and date fields when saving enum snapshots" in
      withEmbeddedMongoDatabase { db =>
        val adjustments = List(
          PriceAdjustment.Unadjusted         -> "none",
          PriceAdjustment.Splits             -> "splits",
          PriceAdjustment.SplitsAndDividends -> "split-and-dividend"
        )
        for
          collection <- db.getCollection("price-observations")
          _          <- collection.insertMany(adjustments.map { case (_, label) =>
            val id = s"legacy-$label"
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
                "provider"         := "TwelveData",
                "currency"         := "USD",
                "adjustment"       := label,
                "fetchedAt"        := original.fetchedAt.toString,
                "requestedFrom"    := from.toString,
                "requestedThrough" := through.toString
              )
            )
          })
          repo    <- PriceHistoryRepository.make[IO](db)
          results <- adjustments.traverse { case (adjustment, label) =>
            val legacy = original.copy(id = s"legacy-$label", adjustment = adjustment)
            val saved  = legacy.copy(id = s"enum-$label", fetchedAt = original.fetchedAt.plusSeconds(1))
            for
              existing <- repo.latest(ticker, adjustment)
              covering <- repo.latestCovering(ticker, adjustment, from)
              _        <- repo.save(saved)
              latest   <- repo.latest(ticker, adjustment)
              raw      <- collection
                .find(
                  Filter.idEq(saved.id) && Filter.eq("adjustment", label) && Filter.eq("data.adjustment", label) &&
                    Filter.eq("data.requestedFrom", from.toString) && Filter.eq("data.requestedThrough", through.toString)
                )
                .first
            yield (legacy, saved, existing, covering, latest, raw.isDefined)
          }
        yield
          results.foreach { case (legacy, saved, existing, covering, latest, preservedSchema) =>
            existing mustBe Some(legacy)
            covering mustBe Some(legacy)
            latest mustBe Some(saved)
            preservedSchema mustBe true
          }
          succeed
      }
  }
}
