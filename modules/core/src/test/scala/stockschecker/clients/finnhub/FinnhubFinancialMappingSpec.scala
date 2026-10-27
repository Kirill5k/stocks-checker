package stockschecker.clients.finnhub

import io.circe.{Json, JsonObject}
import io.circe.parser.parse
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import stockschecker.domain.{FinancialSnapshot, Ticker}

import java.time.Instant
import scala.io.Source
import scala.util.Using

class FinnhubFinancialMappingSpec extends AnyWordSpec with Matchers:
  private val now  = Instant.parse("2026-01-15T12:00:00Z")
  private val body = parse(
    Using.resource(Source.fromResource("finnhub/basic-financials-success.json"))(_.mkString)
  ).toOption.get.asObject.get

  "FinnhubFinancialMapping" should {
    "preserve fiscal histories without confusing cash flow and free cash flow" in {
      val data = FinnhubFinancialMapping.decode(body, Ticker("AAPL"), now).toOption.flatten.get
      data.metrics.freeCashFlowPerShareTtm mustBe Some(BigDecimal("6.6855"))
      data.snapshot.metrics("cashFlowPerShareTTM") mustBe BigDecimal("6.86253")
      data.snapshot.annual.count(_.metric == "roa") mustBe 41
      data.snapshot.quarterly.count(_.metric == "fcfPerShareTTM") mustBe 142
      data.snapshot.annual.find(p => p.metric == "netMargin" && p.period.getYear == 2025).map(_.value) mustBe Some(BigDecimal("0.2692"))
      data.metrics.profitMarginTtm mustBe Some(BigDecimal("26.92"))
      data.snapshot.annual.find(_.metric == "netMargin").map(_.unit) mustBe Some("ratio")
      data.snapshot.validationErrors mustBe Nil
      data.snapshot.rawPayload must include("cashFlowPerShareTTM")
      data.snapshot.payloadHash.length mustBe 64
      data.snapshot.mappingVersion mustBe FinancialSnapshot.FinnhubMappingVersion
      data.snapshot.mappingVersion mustBe "finnhub-basic-financials-v1"
    }

    "keep generic cash flow out of free cash flow when the explicit FCF series is missing" in {
      val data = FinnhubFinancialMapping.decode(body.remove("series"), Ticker("AAPL"), now).toOption.flatten.get
      data.metrics.freeCashFlowPerShareTtm mustBe None
      data.snapshot.metrics("cashFlowPerShareTTM") mustBe BigDecimal("6.86253")
    }

    "record malformed observations and duplicate fiscal periods as audit failures" in {
      val point  = Json.obj("period" -> Json.fromString("2025-09-30"), "v" -> Json.fromBigDecimal(BigDecimal("0.2")))
      val series = Json.obj("annual" -> Json.obj("roa" -> Json.arr(point, point, Json.obj("period" -> Json.fromString("bad")))))
      val data   = FinnhubFinancialMapping.decode(body.add("series", series), Ticker("AAPL"), now).toOption.flatten.get
      data.snapshot.validationErrors must contain allOf ("duplicate-period:annual:roa", "invalid-observation:annual:roa")
    }

    "reject a response without a metric object and preserve a valid no-data response" in {
      FinnhubFinancialMapping.decode(JsonObject.empty, Ticker("AAPL"), now).isLeft mustBe true
      FinnhubFinancialMapping.decode(JsonObject("metric" -> Json.obj()), Ticker("AAPL"), now) mustBe Right(None)
    }

    "keep a stable payload hash but distinct fetch vintages" in {
      val first  = FinnhubFinancialMapping.decode(body, Ticker("AAPL"), now).toOption.flatten.get.snapshot
      val second = FinnhubFinancialMapping.decode(body, Ticker("AAPL"), now.plusSeconds(1)).toOption.flatten.get.snapshot
      first.payloadHash mustBe second.payloadHash
      first.id must not be second.id
    }
  }
