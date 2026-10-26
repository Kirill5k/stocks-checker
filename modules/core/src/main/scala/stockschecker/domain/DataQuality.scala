package stockschecker.domain

import io.circe.Codec
import java.time.Instant

final case class DatasetAvailability(
    ticker: Ticker,
    provider: String,
    dataset: String,
    status: String,
    lastAttemptAt: Instant,
    lastSuccessAt: Option[Instant],
    failure: Option[String]
) derives Codec.AsObject

final case class ExcludedSecurity(ticker: Ticker, reason: String, listingStatus: String) derives Codec.AsObject
final case class DataQualityReport(
    generatedAt: Instant,
    datasets: List[DatasetAvailability],
    exclusions: List[ExcludedSecurity],
    countsByStatus: Map[String, Int],
    rankingCoverage: Map[String, Int],
    rankingEnabled: Boolean,
    coverageAudited: Boolean
) derives Codec.AsObject
