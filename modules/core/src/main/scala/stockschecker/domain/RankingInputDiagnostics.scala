package stockschecker.domain

import io.circe.Codec

import java.time.Instant

final case class RankingInputsQuery(
    industry: Option[String] = None,
    sector: Option[Sector] = None,
    after: Option[Int] = None,
    limit: Int
)

/** Frozen ranking evidence for administrative diagnosis; purged stock rows are removed. */
final case class RankingInputDiagnostic(
    ordinal: Int,
    ticker: Ticker,
    industry: Option[String],
    sector: Option[Sector],
    eligible: Boolean,
    peerGroup: Option[String],
    peerCount: Int,
    exclusions: List[String],
    evidence: Option[InvestmentEvidence]
) derives Codec.AsObject

final case class RankingInputsPage(
    rankingId: InvestmentRankingId,
    asOf: Instant,
    items: List[RankingInputDiagnostic],
    nextAfter: Option[Int]
) derives Codec.AsObject
