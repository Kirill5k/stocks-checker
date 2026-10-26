package stockschecker.clients.finnhub

import io.circe.{Decoder, DecodingFailure, Json, JsonObject}
import stockschecker.domain.{FinancialData, FinancialObservation, FinancialSnapshot, InvestmentScorer, Ticker}

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.time.{Instant, LocalDate}
import scala.util.Try

/** Explicit mapping contract. Unknown numeric fields are preserved but never automatically scored. */
object FinnhubFinancialMapping:
  private val fractions = Set(
    "roa", "roe", "roic", "rotc", "roaTTM", "roeTTM", "roicTTM", "rotcTTM", "netMargin", "operatingMargin",
    "grossMargin", "fcfMargin", "pretaxMargin", "totalDebtToTotalAsset", "totalDebtToEquity", "totalDebtToTotalCapital",
    "longtermDebtTotalAsset", "longtermDebtTotalCapital", "longtermDebtTotalEquity", "netDebtToTotalCapital", "netDebtToTotalEquity"
  )
  private val perShare = Set("eps", "salesPerShare", "fcfPerShareTTM", "ebitPerShare")
  private val multiples = Set("pe", "peTTM", "pfcf", "pfcfTTM", "pb", "ps", "psTTM", "evEbitda", "evEbitdaTTM", "evRevenue", "evRevenueTTM")

  def decode(body: JsonObject, ticker: Ticker, observedAt: Instant): Decoder.Result[Option[FinancialData]] =
    body("metric").toRight(DecodingFailure("Missing financial metric object", Nil)).flatMap(_.as[JsonObject]).flatMap { metric =>
      if metric.isEmpty then Right(None)
      else
        metric.toJson.as[FinnhubClient.FinancialMetricsResponse].map { parsed =>
          val errors = List.newBuilder[String]
          def observations(frequency: String): List[FinancialObservation] =
            val fields = body("series").flatMap(_.asObject).flatMap(_(frequency)).flatMap(_.asObject)
            fields.toList.flatMap(_.toList).flatMap { case (name, values) =>
              values.asArray match
                case None =>
                  errors += s"invalid-series:$frequency:$name"
                  Nil
                case Some(entries) =>
                  val points = entries.toList.flatMap { entry =>
                    val cursor = entry.hcursor
                    val decoded = for
                      period <- cursor.get[String]("period").toOption.flatMap(p => Try(LocalDate.parse(p)).toOption)
                      value <- cursor.get[BigDecimal]("v").toOption
                    yield FinancialObservation(name, period, value, unit(name))
                    // A provider null represents unavailable data; retain it in rawPayload, never manufacture a zero.
                    if decoded.isEmpty && !cursor.downField("v").focus.exists(_.isNull) then
                      errors += s"invalid-observation:$frequency:$name"
                    decoded
                  }
                  if points.map(_.period).distinct.size != points.size then errors += s"duplicate-period:$frequency:$name"
                  points.sortBy(_.period)
            }
          val annual = observations("annual")
          val quarterly = observations("quarterly")
          body("symbol").flatMap(_.asString).filterNot(_.equalsIgnoreCase(ticker.value)).foreach(_ => errors += "response-symbol-mismatch")
          val raw = Json.fromJsonObject(body).noSpaces
          val hash = MessageDigest.getInstance("SHA-256").digest(raw.getBytes(StandardCharsets.UTF_8)).map(b => f"${b & 0xff}%02x").mkString
          val snapshot = FinancialSnapshot(
            id = s"${ticker.value}:$observedAt:$hash",
            ticker = ticker,
            provider = "Finnhub",
            observedAt = observedAt,
            metrics = metric.toList.flatMap { case (key, value) => value.as[BigDecimal].toOption.map(key -> _) }.toMap,
            annual = annual,
            quarterly = quarterly,
            rawPayload = raw,
            payloadHash = hash,
            mappingVersion = InvestmentScorer.MappingVersion,
            validationErrors = errors.result().distinct
          )
          // FCF comes only from the explicitly named TTM FCF series; generic cash flow is retained in the raw snapshot.
          val fcf = quarterly.filter(_.metric == "fcfPerShareTTM").sortBy(_.period).lastOption.map(_.value)
          Some(FinancialData(parsed.toDomain(ticker).copy(freeCashFlowPerShareTtm = fcf), snapshot))
        }
    }

  private def unit(metric: String): String =
    if fractions(metric) then "ratio"
    else if perShare(metric) then "reporting-currency-per-share"
    else if multiples(metric) then "multiple"
    else "provider-defined"
