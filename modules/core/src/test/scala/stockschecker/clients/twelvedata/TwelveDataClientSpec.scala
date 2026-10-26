package stockschecker.clients.twelvedata

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.traverse.*
import io.circe.syntax.*
import kirill5k.common.sttp.test.Sttp4WordSpec
import stockschecker.clients.{ProviderAccess, ProviderLimits}
import stockschecker.common.config.TwelveDataConfig
import stockschecker.domain.{DatasetAvailability, DateRange, PriceAdjustment, Ticker}
import stockschecker.repositories.{DataQualityRepository, ProviderBudgetRepository}
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.WebSocketStreamBackend
import stockschecker.domain.errors.AppError
import sttp.client4.testing.ResponseStub
import sttp.model.{Header, StatusCode}

import java.time.{Instant, LocalDate, ZoneOffset}
import scala.concurrent.duration.*

class TwelveDataClientSpec extends Sttp4WordSpec {
  final private case class AccessFixture(
      access: ProviderAccess[IO],
      reservations: Ref[IO, List[(String, Long, Int)]],
      statuses: Ref[IO, List[String]]
  )

  private def accessFixture: IO[AccessFixture] =
    for
      budgets      <- Ref.of[IO, Map[String, (LocalDate, Int, Instant)]](Map.empty)
      reservations <- Ref.of[IO, List[(String, Long, Int)]](List.empty)
      statuses     <- Ref.of[IO, List[String]](List.empty)
      repository = new ProviderBudgetRepository[IO] {
        def reserve(
            provider: String,
            now: Instant,
            spacingMillis: Long,
            dailyAllowance: Int,
            account: Option[String] = None
        ): IO[Option[Instant]] =
          val id = account.fold(provider)(value => s"$provider:$value")
          reservations.update(_ :+ ((id, spacingMillis, dailyAllowance))) >> budgets.modify { all =>
            val today              = now.atZone(ZoneOffset.UTC).toLocalDate
            val (day, count, next) = all.getOrElse(id, (today, 0, Instant.EPOCH))
            val used               = if day == today then count else 0
            val exhausted          =
              Option.when(dailyAllowance > 0 && used >= dailyAllowance)(today.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant)
            val deferred = (exhausted.toList ++ Option.when(now.isBefore(next))(next)).maxOption
            deferred match
              case Some(until) => (all, Some(until))
              case None        => (all.updated(id, (today, used + 1, now.plusMillis(spacingMillis))), None)
          }
        def cooldown(provider: String, until: Instant, account: Option[String] = None): IO[Unit] =
          val id = account.fold(provider)(value => s"$provider:$value")
          budgets.update { all =>
            val (day, used, next) = all.getOrElse(id, (LocalDate.ofEpochDay(0L), 0, Instant.EPOCH))
            all.updated(id, (day, used, if until.isAfter(next) then until else next))
          }
      }
      quality = new DataQualityRepository[IO] {
        def record(ticker: Ticker, provider: String, dataset: String, status: String, at: Instant, failure: Option[String]): IO[Unit] =
          statuses.update(_ :+ status)
        def all: IO[List[DatasetAvailability]] = IO.pure(List.empty)
      }
      access <- ProviderAccess.make[IO](repository, quality)
    yield AccessFixture(access, reservations, statuses)

  private def makeClient(config: TwelveDataConfig, backend: WebSocketStreamBackend[IO, Fs2Streams[IO]]): IO[TwelveDataClient[IO]] =
    accessFixture.flatMap(fixture => TwelveDataClient.make[IO](config, backend, fixture.access))

  private def dailyResponse(
      values: List[(LocalDate, BigDecimal)],
      symbol: String = "AAPL",
      interval: String = "1day",
      currency: String = "USD",
      metadata: Boolean = true
  ): String = TwelveDataClient
    .TimeSeriesResponse(
      Option.when(metadata)(TwelveDataClient.Meta(symbol, interval, currency, "America/New_York", "NASDAQ", "XNAS", "Common Stock")),
      Some(values.map { case (date, close) => TwelveDataClient.CandleData(date, close, close, close, close, BigDecimal(1000)) }),
      "ok",
      None,
      None
    )
    .asJson
    .noSpaces

  "A TwelveDataClient" when {

    val config = TwelveDataConfig("http://twelvedata.co", "api-key", ProviderLimits(8, 800, 1))

    "getDailyPriceHistory" should {
      val from    = LocalDate.parse("2025-01-02")
      val through = LocalDate.parse("2025-01-06")
      val candles = List(through -> BigDecimal(102), from -> BigDecimal(100))

      "request the explicit adjustment and preserve provider currency and request provenance" in
        List(
          (PriceAdjustment.SplitsAndDividends, "all"),
          (PriceAdjustment.Unadjusted, "none"),
          (PriceAdjustment.Splits, "splits")
        ).traverse { case (requested, wireValue) =>
          val expectedParams = Map(
            "symbol"     -> "AAPL",
            "interval"   -> "1day",
            "apikey"     -> "api-key",
            "outputsize" -> "5000",
            "start_date" -> from.toString,
            "end_date"   -> through.toString,
            "adjust"     -> wireValue
          )
          val backend = fs2BackendStub.whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/time_series") && r.hasParams(expectedParams) =>
              ResponseStub.adjust(dailyResponse(candles, currency = "EUR"))
            case _ => throw new RuntimeException("Unexpected daily history request")
          }
          for {
            client <- makeClient(config, backend)
            result <- client.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), requested)
          } yield (requested, result)
        }.asserting { results =>
          results.foreach { case (adjustment, result) =>
            result.ticker mustBe Ticker("AAPL")
            result.currency mustBe "EUR"
            result.provider mustBe "TwelveData"
            result.adjustment mustBe adjustment
            result.requestedRange mustBe DateRange(from, through)
            result.candles.map(_.date) mustBe List(from, through)
            result.candles.map(_.close) mustBe List(BigDecimal(100), BigDecimal(102))
          }
          succeed
        }

      "treat a single-day range as inclusive in both the request and response" in {
        val day     = DateRange(from, from)
        val backend = fs2BackendStub.whenRequestMatchesPartial {
          case r if r.hasParams(Map("start_date" -> from.toString, "end_date" -> from.toString, "adjust" -> "none")) =>
            ResponseStub.adjust(dailyResponse(List(from -> BigDecimal(100))))
          case _ => throw new AssertionError("Unexpected single-day history request")
        }
        makeClient(config, backend)
          .flatMap(_.getDailyPriceHistory(Ticker("AAPL"), day, PriceAdjustment.Unadjusted))
          .asserting { result =>
            result.candles.map(_.date) mustBe List(from)
            result.requestedRange mustBe day
            result.adjustment mustBe PriceAdjustment.Unadjusted
          }
      }

      "reject incorrect identity, interval, or missing currency metadata" in
        List(
          "wrong symbol"     -> dailyResponse(candles, symbol = "MSFT"),
          "wrong interval"   -> dailyResponse(candles, interval = "1month"),
          "missing metadata" -> dailyResponse(candles, metadata = false)
        ).traverse { case (label, json) =>
          val backend = fs2BackendStub.whenRequestMatchesPartial { case _ => ResponseStub.adjust(json) }
          makeClient(config, backend)
            .flatMap(_.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends))
            .attempt
            .map(label -> _)
        }.asserting { results =>
          results.foreach { case (label, result) =>
            withClue(label)(result.left.toOption.get mustBe a[AppError.FailedValidation])
          }
          succeed
        }

      "reject duplicate, nonpositive, out-of-window and possibly truncated histories" in {
        val start   = LocalDate.parse("2000-01-01")
        val end     = LocalDate.parse("2030-01-01")
        val invalid = List(
          "duplicate"      -> List(start -> BigDecimal(1), start -> BigDecimal(2)),
          "zero"           -> List(start -> BigDecimal(0)),
          "negative"       -> List(start -> BigDecimal(-1)),
          "outside window" -> List(start.minusDays(1) -> BigDecimal(1)),
          "truncated"      -> (0 until 5000).toList.map(i => start.plusDays(i.toLong) -> BigDecimal(1))
        )
        invalid
          .traverse { case (label, values) =>
            val backend = fs2BackendStub.whenRequestMatchesPartial { case _ => ResponseStub.adjust(dailyResponse(values)) }
            makeClient(config, backend)
              .flatMap(_.getDailyPriceHistory(Ticker("AAPL"), DateRange(start, end), PriceAdjustment.SplitsAndDividends))
              .attempt
              .map(label -> _)
          }
          .asserting { results =>
            results.foreach { case (label, result) =>
              withClue(label)(result.left.toOption.get mustBe a[AppError.FailedValidation])
            }
            succeed
          }
      }

      "preserve historical gaps that can represent trading halts" in {
        val values  = List(from.minusMonths(6) -> BigDecimal(90), from -> BigDecimal(100), through -> BigDecimal(102))
        val backend = fs2BackendStub.whenAnyRequest.thenRespond(ResponseStub.adjust(dailyResponse(values)))
        makeClient(config, backend)
          .flatMap(_.getDailyPriceHistory(Ticker("AAPL"), DateRange(from.minusYears(1), through), PriceAdjustment.SplitsAndDividends))
          .asserting(_.candles.map(_.date) mustBe values.map(_._1))
      }

      "return a persisted-work deferral for HTTP 429 with Retry-After without retrying internally" in {
        val backend = fs2BackendStub.whenAnyRequest.thenRespondCyclic(
          ResponseStub.adjust("rate limited", StatusCode.TooManyRequests).copy(headers = Seq(Header("Retry-After", "120"))),
          ResponseStub.adjust(dailyResponse(candles))
        )
        (for {
          client <- makeClient(config, backend)
          before <- IO.realTimeInstant
          first  <- client
            .getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends)
            .attempt
            .timeout(1.second)
          after  <- IO.realTimeInstant
          second <- client.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends).attempt
        } yield (before, after, first, second)).asserting { case (before, after, first, second) =>
          val error = first.left.toOption.get.asInstanceOf[AppError.ProviderDeferred]
          error.provider mustBe "TwelveData"
          error.retryAt.isBefore(before.plusSeconds(120)) mustBe false
          error.retryAt.isAfter(after.plusSeconds(120)) mustBe false
          second.left.toOption.collect { case e: AppError.ProviderDeferred => e.retryAt } mustBe Some(error.retryAt)
        }
      }

      "defer HTTP-200 error bodies with code 429 without retrying internally" in {
        val backend = fs2BackendStub.whenAnyRequest.thenRespondCyclic(
          ResponseStub.adjust("""{"code":429,"status":"error","message":"rate limited"}"""),
          ResponseStub.adjust(dailyResponse(candles))
        )
        (for {
          client <- makeClient(config, backend)
          first  <- client
            .getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends)
            .attempt
            .timeout(1.second)
          second <- client.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends).attempt
        } yield (first, second)).asserting { case (first, second) =>
          first.left.toOption.get mustBe a[AppError.ProviderDeferred]
          second.left.toOption.collect { case e: AppError.ProviderDeferred => e.retryAt } mustBe
            first.left.toOption.collect { case e: AppError.ProviderDeferred => e.retryAt }
        }
      }

      "normalize and rotate distinct API keys with a separate daily allowance for each key" in {
        val multiple = config.copy(apiKey = " key-one, , key-one, key-two, ", limits = ProviderLimits(60000, 1, 1))
        val backend  = fs2BackendStub.whenRequestMatchesPartial {
          case r if r.hasParams(Map("apikey" -> "key-one")) => ResponseStub.adjust(dailyResponse(List(from -> BigDecimal(101))))
          case r if r.hasParams(Map("apikey" -> "key-two")) => ResponseStub.adjust(dailyResponse(List(from -> BigDecimal(202))))
          case _                                            => throw new AssertionError("Unexpected API key")
        }
        (for
          fixture  <- accessFixture
          client   <- TwelveDataClient.make[IO](multiple, backend, fixture.access)
          first    <- client.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends)
          second   <- client.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends)
          third    <- client.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends).attempt
          attempts <- fixture.reservations.get
          statuses <- fixture.statuses.get
        yield (first, second, third, attempts, statuses)).asserting { case (first, second, third, attempts, statuses) =>
          first.candles.head.close mustBe BigDecimal(101)
          second.candles.head.close mustBe BigDecimal(202)
          third.left.toOption.get mustBe a[AppError.ProviderDeferred]
          attempts.size mustBe 4
          attempts.map(_._1).distinct.size mustBe 2
          attempts.foreach { case (account, spacing, allowance) =>
            account must not include "key-one"
            account must not include "key-two"
            spacing mustBe 1L
            allowance mustBe 1
          }
          statuses mustBe List("available", "available")
        }
      }

      "preserve credential budgets when clients are recreated with API keys in a different order" in {
        val multiple = config.copy(apiKey = "key-one,key-two", limits = ProviderLimits(60000, 1, 1))
        val backend  = fs2BackendStub.whenRequestMatchesPartial {
          case r if r.hasParams(Map("apikey" -> "key-one")) => ResponseStub.adjust(dailyResponse(List(from -> BigDecimal(101))))
          case r if r.hasParams(Map("apikey" -> "key-two")) => ResponseStub.adjust(dailyResponse(List(from -> BigDecimal(202))))
          case _                                            => throw new AssertionError("Unexpected API key")
        }
        (for
          fixture   <- accessFixture
          original  <- TwelveDataClient.make[IO](multiple, backend, fixture.access)
          first     <- original.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends)
          reordered <- TwelveDataClient.make[IO](multiple.copy(apiKey = "key-two,key-one"), backend, fixture.access)
          second    <- reordered.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends)
          recreated <- TwelveDataClient.make[IO](multiple, backend, fixture.access)
          exhausted <- recreated.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends).attempt
          attempts  <- fixture.reservations.get
        yield (first, second, exhausted, attempts)).asserting { case (first, second, exhausted, attempts) =>
          first.candles.head.close mustBe BigDecimal(101)
          second.candles.head.close mustBe BigDecimal(202)
          exhausted.left.toOption.get mustBe a[AppError.ProviderDeferred]
          attempts.size mustBe 4
          attempts.map(_._1).distinct.size mustBe 2
        }
      }

      "allow separate keys in flight concurrently while refusing a second request for either occupied key" in {
        val multiple = config.copy(apiKey = "key-one,key-two", limits = ProviderLimits(60000, 0, 1))
        (for
          fixture       <- accessFixture
          firstStarted  <- Deferred[IO, Unit]
          secondStarted <- Deferred[IO, Unit]
          release       <- Deferred[IO, Unit]
          backend = fs2BackendStub
            .whenRequestMatches(_.hasParams(Map("apikey" -> "key-one")))
            .thenRespondF(firstStarted.complete(()).void >> release.get.as(ResponseStub.adjust(dailyResponse(candles))))
            .whenRequestMatches(_.hasParams(Map("apikey" -> "key-two")))
            .thenRespondF(secondStarted.complete(()).void >> release.get.as(ResponseStub.adjust(dailyResponse(candles))))
          client <- TwelveDataClient.make[IO](multiple, backend, fixture.access)
          first  <- client.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends).start
          result <- (for
            _       <- firstStarted.get.timeout(2.seconds)
            second  <- client.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends).start
            results <- (for
              _       <- secondStarted.get.timeout(2.seconds)
              refused <- client.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends).attempt
              _       <- release.complete(())
              firstResult  <- first.joinWithNever
              secondResult <- second.joinWithNever
              reservations <- fixture.reservations.get
            yield (refused, firstResult, secondResult, reservations)).guarantee(second.cancel)
          yield results).guarantee(first.cancel)
        yield result).asserting { case (refused, first, second, reservations) =>
          refused.left.toOption.get mustBe a[AppError.ProviderDeferred]
          first.candles.size mustBe 2
          second.candles.size mustBe 2
          reservations.size mustBe 2
          reservations.map(_._1).distinct.size mustBe 2
        }
      }

      "enforce each key's minute limit on direct client calls" in {
        val multiple = config.copy(apiKey = "key-one,key-two", limits = ProviderLimits(1, 0, 1))
        val backend  = fs2BackendStub.whenAnyRequest.thenRespond(ResponseStub.adjust(dailyResponse(candles)))
        (for
          fixture  <- accessFixture
          client   <- TwelveDataClient.make[IO](multiple, backend, fixture.access)
          before   <- IO.realTimeInstant
          _        <- client.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends)
          _        <- client.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends)
          result   <- client.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends).attempt
          after    <- IO.realTimeInstant
          attempts <- fixture.reservations.get
        yield (before, after, result, attempts)).asserting { case (before, after, result, attempts) =>
          val error = result.left.toOption.get.asInstanceOf[AppError.ProviderDeferred]
          error.retryAt.isBefore(before.plusSeconds(60)) mustBe false
          error.retryAt.isAfter(after.plusSeconds(60)) mustBe false
          attempts.map(_._2).distinct mustBe List(60000L)
        }
      }

      "continue with a healthy key after an authentication failure and record only the successful logical request" in {
        val multiple = config.copy(apiKey = "invalid-secret,healthy-secret", limits = ProviderLimits(60000, 0, 1))
        val backend  = fs2BackendStub.whenRequestMatchesPartial {
          case r if r.hasParams(Map("apikey" -> "invalid-secret")) => ResponseStub.adjust("invalid-secret", StatusCode.Unauthorized)
          case r if r.hasParams(Map("apikey" -> "healthy-secret")) => ResponseStub.adjust(dailyResponse(candles))
          case _                                                   => throw new AssertionError("Unexpected API key")
        }
        (for
          fixture  <- accessFixture
          client   <- TwelveDataClient.make[IO](multiple, backend, fixture.access)
          first    <- client.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends)
          _        <- IO.sleep(3.millis)
          _        <- client.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends)
          _        <- IO.sleep(3.millis)
          _        <- client.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends)
          attempts <- fixture.reservations.get
          statuses <- fixture.statuses.get
        yield (first, attempts, statuses)).asserting { case (first, attempts, statuses) =>
          first.candles.size mustBe 2
          attempts.groupBy(_._1).values.map(_.size).toList.sorted mustBe List(1, 3)
          statuses mustBe List("available", "available", "available")
        }
      }

      "return the earliest available key deadline when all keys are rate limited" in {
        val multiple = config.copy(apiKey = "slow-secret,fast-secret")
        val backend  = fs2BackendStub.whenRequestMatchesPartial {
          case r if r.hasParams(Map("apikey" -> "slow-secret")) =>
            ResponseStub.adjust("slow-secret", StatusCode.TooManyRequests).copy(headers = Seq(Header("Retry-After", "120")))
          case r if r.hasParams(Map("apikey" -> "fast-secret")) =>
            ResponseStub.adjust("fast-secret", StatusCode.TooManyRequests).copy(headers = Seq(Header("Retry-After", "60")))
          case _ => throw new AssertionError("Unexpected API key")
        }
        (for
          fixture  <- accessFixture
          client   <- TwelveDataClient.make[IO](multiple, backend, fixture.access)
          before   <- IO.realTimeInstant
          first    <- client.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends).attempt
          after    <- IO.realTimeInstant
          second   <- client.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends).attempt
          statuses <- fixture.statuses.get
        yield (before, after, first, second, statuses)).asserting { case (before, after, first, second, statuses) =>
          val error = first.left.toOption.get.asInstanceOf[AppError.ProviderDeferred]
          error.retryAt.isBefore(before.plusSeconds(60)) mustBe false
          error.retryAt.isAfter(after.plusSeconds(60)) mustBe false
          second.left.toOption.collect { case e: AppError.ProviderDeferred => e.retryAt } mustBe Some(error.retryAt)
          error.toString must not include "slow-secret"
          error.toString must not include "fast-secret"
          statuses mustBe empty
        }
      }

      "return a configuration error without exposing keys when every key is invalid" in {
        val multiple = config.copy(apiKey = "first-secret,second-secret")
        val backend  = fs2BackendStub.whenAnyRequest.thenRespond(ResponseStub.adjust("first-secret,second-secret", StatusCode.Forbidden))
        (for
          fixture  <- accessFixture
          client   <- TwelveDataClient.make[IO](multiple, backend, fixture.access)
          first    <- client.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends).attempt
          second   <- client.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends).attempt
          attempts <- fixture.reservations.get
          statuses <- fixture.statuses.get
        yield (first, second, attempts, statuses)).asserting { case (first, second, attempts, statuses) =>
          List(first, second).foreach { result =>
            val error = result.left.toOption.get
            error mustBe a[AppError.ProviderConfiguration]
            error.toString must not include "first-secret"
            error.toString must not include "second-secret"
          }
          attempts.size mustBe 2
          statuses mustBe List("configuration-error", "configuration-error")
        }
      }

      "redact configured API keys from provider error bodies" in {
        val multiple = config.copy(apiKey = "first-secret,second-secret")
        val backend  = fs2BackendStub.whenAnyRequest.thenRespond(
          ResponseStub.adjust("failure for first-secret and second-secret", StatusCode.InternalServerError)
        )
        makeClient(multiple, backend)
          .flatMap(_.getDailyPriceHistory(Ticker("AAPL"), DateRange(from, through), PriceAdjustment.SplitsAndDividends))
          .attempt
          .asserting { result =>
            val error = result.left.toOption.get
            error mustBe a[AppError.HttpClient]
            error.toString must not include "first-secret"
            error.toString must not include "second-secret"
          }
      }

      "reject blank API keys and invalid limits during construction" in {
        val backend = fs2BackendStub.whenAnyRequest.thenRespond(ResponseStub.adjust(dailyResponse(candles)))
        List(
          config.copy(apiKey = " , , "),
          config.copy(limits = config.limits.copy(requestsPerMinute = 0)),
          config.copy(limits = config.limits.copy(dailyAllowance = -1)),
          config.copy(limits = config.limits.copy(maxConcurrent = 0))
        ).traverse(makeClient(_, backend).attempt).asserting { results =>
          results.head.left.toOption.get mustBe a[AppError.Critical]
          results.tail.foreach(_.left.toOption.get mustBe a[IllegalArgumentException])
          succeed
        }
      }

      "reject invalid date ranges before issuing requests" in {
        val backend = fs2BackendStub.whenRequestMatchesPartial { case _ =>
          throw new RuntimeException("An invalid request reached the provider")
        }
        (for {
          fixture  <- accessFixture
          client   <- TwelveDataClient.make[IO](config, backend, fixture.access)
          result   <- client.getDailyPriceHistory(Ticker("AAPL"), DateRange(through, from), PriceAdjustment.SplitsAndDividends).attempt
          attempts <- fixture.reservations.get
          statuses <- fixture.statuses.get
        } yield (result, attempts, statuses)).asserting { case (result, attempts, statuses) =>
          result.left.toOption.get mustBe a[AppError.FailedValidation]
          attempts mustBe empty
          statuses mustBe empty
        }
      }
    }

  }
}
