package stockschecker.repositories

import cats.MonadThrow
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import kirill5k.common.cats.syntax.applicative.*
import kirill5k.common.cats.Clock
import kirill5k.common.syntax.time.*
import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.IndexOptions
import mongo4cats.operations.{Aggregate, Filter, Index, Projection, Sort, Update}
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.repositories.entities.{
  InvestmentCandidateEntity,
  InvestmentRankingEntity,
  InvestmentRankingIdEntity,
  InvestmentRankingInputEntity
}
import stockschecker.repositories.queries.{InvestmentRankingQueries, StockAggregates}

import java.time.Instant
import java.util.concurrent.TimeUnit.SECONDS

trait InvestmentRankingRepository[F[_]]:
  def findCandidates: F[List[InvestmentCandidate]]
  def rankingInputs(id: InvestmentRankingId, query: RankingInputsQuery): F[RankingInputsPage]
  def save(ranking: InvestmentRanking, entries: List[InvestmentRankingEntry]): F[Unit]
  def latest: F[Option[InvestmentRanking]]
  def delete(ticker: Ticker): F[Unit]
  def pin(id: InvestmentRankingId): F[Unit]
  def release(id: InvestmentRankingId, requestedAt: Instant): F[Unit]

final private class LiveInvestmentRankingRepository[F[_]](
    private val collection: MongoCollection[F, InvestmentRankingEntity],
    private val inputs: MongoCollection[F, InvestmentRankingInputEntity],
    private val securities: MongoCollection[F, InvestmentCandidateEntity]
)(using
    F: MonadThrow[F],
    C: Clock[F]
) extends InvestmentRankingRepository[F] {
  import InvestmentRankingRepository.Field

  override def findCandidates: F[List[InvestmentCandidate]] =
    val stock = StockAggregates.Field
    securities
      .aggregate[InvestmentCandidateEntity](
        StockAggregates
          .build(Filter.empty, StockFilters(), None)
          .replaceWith(Document(Field.Stock := "$$ROOT"))
          .lookup(
            FinancialHistoryRepository.CollectionName,
            s"${Field.Stock}.${stock.Security}.${stock.Id}",
            stock.Id,
            Field.FinancialSnapshots
          )
          .sort(Sort.asc(s"${Field.Stock}.${stock.Security}.${stock.Id}"))
      )
      .all
      .mapList(_.toDomain)

  override def rankingInputs(id: InvestmentRankingId, query: RankingInputsQuery): F[RankingInputsPage] =
    val filter = List(
      Some(Filter.eq(Field.RankingId, id.value)),
      query.after.map(ordinal => Filter.gt(Field.Ordinal, ordinal)),
      query.industry.map(industry => Filter.eq(Field.InputIndustry, InvestmentScreenInput.industryKey(industry))),
      query.sector.map(sector => Filter.eq(Field.InputSector, sector.print))
    ).flatten.foldLeft(Filter.empty)(_ && _)
    for
      now    <- C.now
      header <- collection
        .find(Filter.idEq(id.value) && InvestmentRankingQueries.notExpired(now))
        .first
        .flatMap(F.fromOption(_, AppError.EntityDoesNotExist("Investment ranking", id.value)))
      rows <- inputs.find(filter).sort(Sort.asc(Field.Ordinal)).limit(query.limit + 1).all
      page = rows.take(query.limit).map(_.toDiagnostic).toList
    yield RankingInputsPage(id, header.createdAt, page, page.lastOption.filter(_ => rows.size > query.limit).map(_.ordinal))

  override def save(ranking: InvestmentRanking, entries: List[InvestmentRankingEntry]): F[Unit] =
    val rows = entries.zipWithIndex.map { case (entry, ordinal) =>
      InvestmentRankingInputEntity(
        s"${ranking.id.value}:${entry.scorecard.ticker.value}",
        ranking.id,
        ordinal,
        entry.input.copy(industry = entry.input.industry.map(InvestmentScreenInput.industryKey)),
        entry.scorecard,
        ranking.createdAt,
        Some(ranking.createdAt.plus(InvestmentRecipe.RankingRetention))
      )
    }
    // Publish metadata only after every input batch is durable. The universe can exceed MongoDB's document size limit.
    rows
      .grouped(100)
      .toList
      .traverse_(batch => inputs.insertMany(batch).void) >>
      collection.insertOne(InvestmentRankingEntity.from(ranking)).void

  override def latest: F[Option[InvestmentRanking]] =
    C.now.flatMap(now => collection.aggregate[InvestmentRankingEntity](InvestmentRankingQueries.latest(now)).first.mapOpt(_.toDomain))

  override def delete(ticker: Ticker): F[Unit] =
    inputs.deleteMany(Filter.eq(Field.ScorecardTicker, ticker.value)).void

  override def pin(id: InvestmentRankingId): F[Unit] =
    inputs.updateMany(Filter.eq(Field.RankingId, id.value), Update.unset(Field.ExpiresAt)).void >>
      collection.updateOne(Filter.idEq(id.value), Update.unset(Field.ExpiresAt)).void

  private def hasReferences(id: InvestmentRankingId): F[Boolean] =
    collection
      .aggregate[InvestmentRankingIdEntity](
        Aggregate
          .matchBy(Filter.idEq(id.value))
          .lookup(InvestmentScreenRepository.CollectionName, Field.Id, InvestmentScreenRepository.Field.RankingId, Field.ScreenReferences)
          .matchBy(Filter.size(Field.ScreenReferences, 0).not)
          .project(Projection.include(Field.Id))
      )
      .first
      .map(_.isDefined)

  override def release(id: InvestmentRankingId, requestedAt: Instant): F[Unit] =
    for
      referenced <- hasReferences(id)
      _          <-
        if referenced then pin(id)
        else
          collection.find(Filter.idEq(id.value)).first.flatMap {
            case None         => F.unit
            case Some(header) =>
              C.now.flatMap { now =>
                val retainedUntil = header.createdAt.plus(InvestmentRecipe.RankingRetention)
                val expiresAt     = if now.isAfter(retainedUntil) then now else retainedUntil
                val update        = Update.set(Field.ExpiresAt, expiresAt)
                inputs.updateMany(Filter.eq(Field.RankingId, id.value), update).void >>
                  collection.updateOne(Filter.idEq(id.value), update).void >>
                  hasReferences(id).flatMap(referenced => F.whenA(referenced)(pin(id)))
              }
          }
    yield ()

}

object InvestmentRankingRepository extends MongoJsonCodecs:
  val CollectionName       = "investment-rankings"
  val InputsCollectionName = "investment-ranking-inputs"

  object Field:
    val Id                 = "_id"
    val CreatedAt          = "createdAt"
    val RankingId          = "rankingId"
    val Ordinal            = "ordinal"
    val ScreenReferences   = "screenReferences"
    val ExpiresAt          = "expiresAt"
    val ScorecardTicker    = "scorecard.ticker"
    val InputIndustry      = "input.industry"
    val InputSector        = "input.sector"
    val Stock              = "stock"
    val FinancialSnapshots = "financialSnapshots"

  def make[F[_]: {MonadThrow, Clock}](db: MongoDatabase[F]): F[InvestmentRankingRepository[F]] =
    for
      rawCollection <- db.getCollectionWithCodec[InvestmentRankingEntity](CollectionName)
      collection = rawCollection.withAddedCodec[InvestmentRankingIdEntity]
      rawInputs <- db.getCollectionWithCodec[InvestmentRankingInputEntity](InputsCollectionName)
      inputs = rawInputs.withAddedCodec[InvestmentRankingIdEntity]
      securities <- db.getCollectionWithCodec[InvestmentCandidateEntity](SecurityRepository.CollectionName)
      _          <- collection.createIndex(Index.descending(Field.CreatedAt))
      _          <- inputs.createIndex(Index.ascending(List(Field.RankingId, Field.Ordinal)))
      _          <- inputs.createIndex(Index.ascending(Field.ScorecardTicker))
      _          <- collection.createIndex(Index.ascending(Field.ExpiresAt), IndexOptions().expireAfter(0L, SECONDS))
      _          <- inputs.createIndex(Index.ascending(Field.ExpiresAt), IndexOptions().expireAfter(0L, SECONDS))
    yield LiveInvestmentRankingRepository[F](collection, inputs, securities)
