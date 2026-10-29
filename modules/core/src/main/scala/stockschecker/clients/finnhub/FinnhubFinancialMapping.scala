package stockschecker.clients.finnhub

import io.circe.{Decoder, DecodingFailure, Json, JsonObject}
import stockschecker.domain.*

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.time.{Instant, LocalDate}
import scala.util.Try

/** Translates the supported provider fields and units into the canonical financial model. */
object FinnhubFinancialMapping:
  val Version = "finnhub-basic-financials-v2"

  private val currentMetrics = Map(
    "peTTM"           -> FinancialMetric.PriceToEarningsTtm,
    "pfcfShareTTM"    -> FinancialMetric.PriceToFreeCashFlowTtm,
    "epsTTM"          -> FinancialMetric.EpsTtm,
    "revenueGrowth5Y" -> FinancialMetric.RevenueGrowth5Y,
    "epsGrowth5Y"     -> FinancialMetric.EpsGrowth5Y
  )
  private val annualMetrics = Map(
    "roa"                   -> FinancialMetric.ReturnOnAssets,
    "fcfMargin"             -> FinancialMetric.FreeCashFlowMargin,
    "totalDebtToTotalAsset" -> FinancialMetric.DebtToAssets
  )
  private val quarterlyMetrics = Map("fcfPerShareTTM" -> FinancialMetric.FreeCashFlowPerShareTtm)

  def decode(body: JsonObject, ticker: Ticker, observedAt: Instant): Decoder.Result[Option[FinancialData]] =
    body("metric").toRight(DecodingFailure("Missing financial metric object", Nil)).flatMap(_.as[JsonObject]).flatMap { metric =>
      if metric.isEmpty then Right(None)
      else
        metric.toJson.as[FinnhubClient.FinancialMetricsResponse].map { parsed =>
          val issues = List.newBuilder[FinancialSnapshotIssue]
          def observations(frequency: String, mapping: Map[String, FinancialMetric]): List[FinancialObservation] =
            val fields = body("series").flatMap(_.asObject).flatMap(_(frequency)).flatMap(_.asObject)
            fields.toList.flatMap(_.toList).flatMap { case (name, values) =>
              mapping.get(name).toList.flatMap { canonical =>
                values.asArray match
                  case None =>
                    issues += FinancialSnapshotIssue.InvalidSeries(frequency, canonical)
                    Nil
                  case Some(entries) =>
                    val points = entries.toList.flatMap { entry =>
                      val cursor  = entry.hcursor
                      val decoded = for
                        period <- cursor.get[String]("period").toOption.flatMap(p => Try(LocalDate.parse(p)).toOption)
                        value  <- cursor.get[BigDecimal]("v").toOption
                      yield FinancialObservation(canonical, period, value)
                      // Null means unavailable; do not invent a zero.
                      if decoded.isEmpty && !cursor.downField("v").focus.exists(_.isNull) then
                        issues += FinancialSnapshotIssue.InvalidObservation(frequency, canonical)
                      decoded
                    }
                    if points.map(_.period).distinct.size != points.size then
                      issues += FinancialSnapshotIssue.DuplicatePeriod(frequency, canonical)
                    points.sortBy(_.period)
              }
            }
          val annual    = observations("annual", annualMetrics)
          val quarterly = observations("quarterly", quarterlyMetrics)
          body("symbol")
            .flatMap(_.asString)
            .filterNot(_.equalsIgnoreCase(ticker.value))
            .foreach(_ => issues += FinancialSnapshotIssue.SymbolMismatch)
          val raw  = Json.fromJsonObject(body).noSpaces
          val hash = MessageDigest.getInstance("SHA-256").digest(raw.getBytes(StandardCharsets.UTF_8)).map(b => f"${b & 0xff}%02x").mkString
          val metrics = metric.toList.flatMap { case (key, value) =>
            for
              canonical <- currentMetrics.get(key)
              number    <- value.as[BigDecimal].toOption
            yield canonical -> (if canonical.unit == MetricUnit.Ratio then number / 100 else number)
          }.toMap
          val snapshot = FinancialSnapshot(
            ticker = ticker,
            provider = Provider.Finnhub,
            observedAt = observedAt,
            metrics = metrics,
            annual = annual,
            quarterly = quarterly,
            payloadHash = hash,
            mappingVersion = Version,
            issues = issues.result().distinct
          )
          val fcf = quarterly.filter(_.metric == FinancialMetric.FreeCashFlowPerShareTtm).sortBy(_.period).lastOption.map(_.value)
          Some(FinancialData(parsed.toDomain(ticker).copy(freeCashFlowPerShareTtm = fcf), snapshot))
        }
    }
