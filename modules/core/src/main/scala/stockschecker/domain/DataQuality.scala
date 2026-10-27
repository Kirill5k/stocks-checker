package stockschecker.domain

import io.circe.Codec
import java.time.Instant

final case class DatasetAvailability(
    ticker: Ticker,
    provider: Provider,
    dataset: Dataset,
    status: DatasetStatus,
    lastAttemptAt: Instant,
    lastSuccessAt: Option[Instant],
    failure: Option[String]
) derives Codec.AsObject

final case class ExcludedSecurity(
    ticker: Ticker,
    reason: String,
    listingStatus: ListingStatus
) derives Codec.AsObject

final case class DataQualityReport(
    generatedAt: Instant,
    datasets: List[DatasetAvailability],
    exclusions: List[ExcludedSecurity],
    countsByStatus: Map[DatasetStatus, Int],
    rankingCoverage: Map[String, Int],
    rankingAsOf: Option[Instant] = None
) derives Codec.AsObject
