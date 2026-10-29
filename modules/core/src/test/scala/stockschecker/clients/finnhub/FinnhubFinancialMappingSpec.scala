package stockschecker.clients.finnhub

import io.circe.{Json, JsonObject}
import io.circe.parser.parse
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import stockschecker.domain.*

import java.time.Instant
import scala.io.Source
import scala.util.Using

class FinnhubFinancialMappingSpec extends AnyWordSpec with Matchers:
  private val now  = Instant.parse("2026-01-15T12:00:00Z")
  private val body = parse(
    Using.resource(Source.fromResource("finnhub/basic-financials-success.json"))(_.mkString)
  ).toOption.get.asObject.get

  "FinnhubFinancialMapping" should {
    "translate supported keys and units while retaining provenance of the complete response" in {
      val data = FinnhubFinancialMapping.decode(body, Ticker("AAPL"), now).toOption.flatten.get
      data.metrics.freeCashFlowPerShareTtm mustBe Some(BigDecimal("6.6855"))
      data.snapshot.metrics.keySet mustBe Set(
        FinancialMetric.PriceToEarningsTtm,
        FinancialMetric.PriceToFreeCashFlowTtm,
        FinancialMetric.EpsTtm,
        FinancialMetric.RevenueGrowth5Y,
        FinancialMetric.EpsGrowth5Y
      )
      data.snapshot
        .metrics(FinancialMetric.PriceToFreeCashFlowTtm) mustBe body("metric").get.hcursor.get[BigDecimal]("pfcfShareTTM").toOption.get
      data.snapshot.metrics(FinancialMetric.RevenueGrowth5Y) mustBe BigDecimal("0.0868")
      data.snapshot.metrics(FinancialMetric.EpsGrowth5Y) mustBe BigDecimal("0.1791")
      data.snapshot.annual.count(_.metric == FinancialMetric.ReturnOnAssets) mustBe 41
      data.snapshot.quarterly.count(_.metric == FinancialMetric.FreeCashFlowPerShareTtm) mustBe 142
      data.snapshot.annual.map(_.unit).distinct mustBe List(MetricUnit.Ratio)
      data.snapshot.quarterly.map(_.unit).distinct mustBe List(MetricUnit.PerShare)
      FinancialMetric.PriceToFreeCashFlowTtm.unit mustBe MetricUnit.Multiple
      data.snapshot.issues mustBe Nil
      data.snapshot.payloadHash.length mustBe 64
      data.snapshot.mappingVersion mustBe FinnhubFinancialMapping.Version
    }

    "keep generic cash flow out of free cash flow when the explicit FCF series is missing" in {
      val data = FinnhubFinancialMapping.decode(body.remove("series"), Ticker("AAPL"), now).toOption.flatten.get
      data.metrics.freeCashFlowPerShareTtm mustBe None
    }

    "record typed malformed observations, invalid series, duplicate periods and symbol mismatches" in {
      val point  = Json.obj("period" -> Json.fromString("2025-09-30"), "v" -> Json.fromBigDecimal(BigDecimal("0.2")))
      val series = Json.obj(
        "annual" -> Json.obj(
          "roa"       -> Json.arr(point, point, Json.obj("period" -> Json.fromString("bad"))),
          "fcfMargin" -> Json.fromString("invalid")
        )
      )
      val data = FinnhubFinancialMapping
        .decode(body.add("series", series).add("symbol", Json.fromString("MSFT")), Ticker("AAPL"), now)
        .toOption
        .flatten
        .get
      data.snapshot.issues must contain allOf (
        FinancialSnapshotIssue.DuplicatePeriod("annual", FinancialMetric.ReturnOnAssets),
        FinancialSnapshotIssue.InvalidObservation("annual", FinancialMetric.ReturnOnAssets),
        FinancialSnapshotIssue.InvalidSeries("annual", FinancialMetric.FreeCashFlowMargin),
        FinancialSnapshotIssue.SymbolMismatch
      )
    }

    "ignore malformed unmapped fields and unavailable null observations" in {
      val series = Json.obj(
        "annual" -> Json.obj(
          "unused" -> Json.fromString("invalid"),
          "eps"    -> Json.arr(Json.obj("period" -> Json.fromString("bad"))),
          "roa"    -> Json.arr(Json.obj("period" -> Json.fromString("2025-09-30"), "v" -> Json.Null))
        )
      )
      val data = FinnhubFinancialMapping.decode(body.add("series", series), Ticker("AAPL"), now).toOption.flatten.get
      data.snapshot.issues mustBe Nil
      data.snapshot.annual mustBe Nil
    }

    "reject a response without a metric object and preserve a valid no-data response" in {
      FinnhubFinancialMapping.decode(JsonObject.empty, Ticker("AAPL"), now).isLeft mustBe true
      FinnhubFinancialMapping.decode(JsonObject("metric" -> Json.obj()), Ticker("AAPL"), now) mustBe Right(None)
    }

    "keep a stable payload hash but distinct fetch vintages" in {
      val first  = FinnhubFinancialMapping.decode(body, Ticker("AAPL"), now).toOption.flatten.get.snapshot
      val second = FinnhubFinancialMapping.decode(body, Ticker("AAPL"), now.plusSeconds(1)).toOption.flatten.get.snapshot
      first.payloadHash mustBe second.payloadHash
      first.observedAt mustBe now
      second.observedAt mustBe now.plusSeconds(1)
    }

    "include unmapped provider fields in the payload hash without retaining the response" in {
      val first   = FinnhubFinancialMapping.decode(body, Ticker("AAPL"), now).toOption.flatten.get.snapshot
      val changed = FinnhubFinancialMapping
        .decode(body.add("unmapped", Json.fromString("provider detail")), Ticker("AAPL"), now)
        .toOption
        .flatten
        .get
        .snapshot
      changed.metrics mustBe first.metrics
      changed.annual mustBe first.annual
      changed.quarterly mustBe first.quarterly
      changed.payloadHash must not be first.payloadHash
    }
  }
