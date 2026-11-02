package stockschecker.repositories

import stockschecker.domain.DatasetStatus
import stockschecker.domain.Provider
import cats.effect.Concurrent
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import mongo4cats.circe.MongoJsonCodecs
import stockschecker.repositories.entities.{DataQualityReportEntity, DatasetAvailabilityEntity}
import stockschecker.repositories.queries.{InvestmentRankingQueries, PriceAnalyticsReadinessQuery}
import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.UpdateOptions
import mongo4cats.operations.{Accumulator, Aggregate, Filter, Index, Projection, Sort, Update}
import stockschecker.domain.{DataQualityReport, Dataset, InvestmentRecipe, Ticker}
import stockschecker.domain.investment.InvestmentReasons
import java.time.Instant

trait DataQualityRepository[F[_]]:
  def record(
      ticker: Ticker,
      provider: Provider,
      dataset: Dataset,
      status: DatasetStatus,
      at: Instant,
      failure: Option[String]
  ): F[Unit]
  def report(at: Instant, recipe: InvestmentRecipe): F[DataQualityReport]

final private class LiveDataQualityRepository[F[_]: Concurrent](
    collection: MongoCollection[F, DatasetAvailabilityEntity]
) extends DataQualityRepository[F] {
  import DataQualityRepository.Field

  private def reportAggregation(at: Instant): Aggregate =
    val security = SecurityRepository.Field
    val ranking  = InvestmentRankingRepository.Field
    // The facet provides a result even before any availability has been recorded.
    Aggregate
      .facet(
        Aggregate.Facet(Field.Datasets, Aggregate.sort(Sort.asc(Field.Ticker).asc(Field.Dataset).asc(Field.Provider))),
        Aggregate.Facet(Field.CountsByStatus, Aggregate.group(s"$$${Field.Status}", Accumulator.sum(Field.Count, 1)))
      )
      .lookup(
        SecurityRepository.CollectionName,
        Aggregate
          .matchBy(Filter.isNull(security.ExclusionReason).not)
          .sort(Sort.asc(security.Ticker))
          .project(
            Projection
              .include(List(security.Ticker, security.ListingStatus))
              .computed(Field.Reason, s"$$${security.ExclusionReason}")
              .excludeId
          ),
        Field.Exclusions
      )
      .lookup(
        InvestmentRankingRepository.CollectionName,
        Aggregate
          .matchBy(InvestmentRankingQueries.notExpired(at))
          .sort(Sort.desc(ranking.CreatedAt))
          .limit(1)
          .facet(
            Aggregate.Facet(Field.RankingHeaders, Aggregate.limit(1)),
            Aggregate.Facet(
              Field.PeerCounts,
              Aggregate
                .lookup(InvestmentRankingRepository.InputsCollectionName, ranking.Id, ranking.RankingId, Field.RankingInputs)
                // Keep these adjacent so Mongo coalesces the lookup and unwind instead of embedding all scorecards.
                .unwind(s"$$${Field.RankingInputs}")
                .matchBy(
                  Filter.eq(s"${Field.RankingInputs}.scorecard.eligible", true) ||
                    Filter.eq(s"${Field.RankingInputs}.scorecard.exclusions", List(InvestmentReasons.InsufficientPeerCoverage))
                )
                .group(
                  Document(
                    "industry" := s"$$${Field.RankingInputs}.${ranking.InputIndustry}",
                    "sector"   := s"$$${Field.RankingInputs}.input.sector"
                  ),
                  Accumulator.sum(Field.Count, 1)
                )
            )
          ),
        Field.Rankings
      )
      .lookup(PriceAnalyticsRepository.CollectionName, PriceAnalyticsReadinessQuery.pipeline, Field.PriceAnalyticsReadiness)

  override def record(
      ticker: Ticker,
      provider: Provider,
      dataset: Dataset,
      status: DatasetStatus,
      at: Instant,
      failure: Option[String]
  ): F[Unit] = {
    val id     = s"${provider.print}:${dataset.print}:${ticker.value}"
    val update = Update
      .setOnInsert(Field.Id, id)
      .set(Field.Ticker, ticker)
      .set(Field.Provider, provider.print)
      .set(Field.Dataset, dataset.print)
      .set(Field.Status, status.print)
      .set(Field.LastAttemptAt, at)
      .set(Field.Failure, failure)
    val dated = if (status == DatasetStatus.Available) update.set(Field.LastSuccessAt, at) else update
    collection.updateOne(Filter.idEq(id), dated, UpdateOptions(upsert = true)).void
  }

  override def report(at: Instant, recipe: InvestmentRecipe): F[DataQualityReport] =
    collection
      .aggregate[DataQualityReportEntity](reportAggregation(at))
      .first
      .map(_.fold(DataQualityReport(at, Nil, Nil, Map.empty, Map.empty))(_.toDomain(at, recipe)))
}

object DataQualityRepository extends MongoJsonCodecs {
  val CollectionName = "dataset-availability"

  object Field:
    val Id                      = "_id"
    val Ticker                  = "ticker"
    val Provider                = "provider"
    val Dataset                 = "dataset"
    val Status                  = "status"
    val LastAttemptAt           = "lastAttemptAt"
    val LastSuccessAt           = "lastSuccessAt"
    val Failure                 = "failure"
    val Datasets                = "datasets"
    val CountsByStatus          = "countsByStatus"
    val Count                   = "count"
    val Exclusions              = "exclusions"
    val Reason                  = "reason"
    val Rankings                = "rankings"
    val RankingHeaders          = "headers"
    val RankingInputs           = "inputs"
    val PeerCounts              = "peerCounts"
    val PriceAnalyticsReadiness = "priceAnalyticsReadiness"

  def make[F[_]: Concurrent](db: MongoDatabase[F]): F[DataQualityRepository[F]] =
    for
      collection <- db.getCollectionWithCodec[DatasetAvailabilityEntity](CollectionName)
      _          <- collection.createIndex(Index.ascending(Field.Status))
    yield new LiveDataQualityRepository[F](collection.withAddedCodec[Ticker].withAddedCodec[DataQualityReportEntity])

}
