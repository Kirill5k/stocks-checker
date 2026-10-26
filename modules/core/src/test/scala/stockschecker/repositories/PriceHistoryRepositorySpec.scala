package stockschecker.repositories

import cats.effect.IO
import stockschecker.domain.*
import java.time.{Instant, LocalDate}

class PriceHistoryRepositorySpec extends RepositorySpec {
  override def port: Int = 12163
  private val ticker = Ticker("AAA")
  private val from = LocalDate.parse("2025-01-01")
  private val through = LocalDate.parse("2025-02-01")
  private val bar = PriceCandle(from.plusDays(1), 100, 100, 100, 100, 1L)
  private val original = PriceHistorySnapshot("price-vintage-1", ticker, List(bar), "TwelveData", "USD", "split-and-dividend",
    Instant.parse("2025-02-02T12:00:00Z"), from, through)

  "Price history repository" should {
    "preserve immutable vintages and find only histories covering the requested starting date" in {
      withEmbeddedMongoDatabase { db =>
        for
          repo <- PriceHistoryRepository.make[IO](db)
          _ <- repo.save(original)
          _ <- repo.save(original.copy(currency = "EUR"))
          latest <- repo.latest(ticker, "split-and-dividend")
          covered <- repo.latestCovering(ticker, "split-and-dividend", from.plusDays(1))
          before <- repo.latestCovering(ticker, "split-and-dividend", from.minusDays(1))
        yield {
          latest mustBe Some(original)
          covered mustBe Some(original)
          before mustBe None
        }
      }
    }
  }
}
