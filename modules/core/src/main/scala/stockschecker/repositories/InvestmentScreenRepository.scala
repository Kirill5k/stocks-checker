package stockschecker.repositories

import cats.MonadThrow
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Aggregate, Filter, Index, Sort}
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.repositories.entities.{
  InvestmentRankingEntity,
  InvestmentRankingInputEntity,
  InvestmentScreenEntity,
  InvestmentScreenWithRankingEntity
}

trait InvestmentScreenRepository[F[_]]:
  def findWithRanking(id: InvestmentScreenId): F[(InvestmentScreen, InvestmentRanking)]
  def latestRanking: F[Option[InvestmentRanking]]
  def rankingEntries(id: InvestmentRankingId): F[List[InvestmentRankingEntry]]
  def save(screen: InvestmentScreen): F[Unit]
  def get(id: InvestmentScreenId): F[Option[InvestmentScreen]]
  def list(limit: Int): F[List[InvestmentScreenSummary]]

final private class LiveInvestmentScreenRepository[F[_]](
    private val collection: MongoCollection[F, InvestmentScreenEntity],
    private val rankings: MongoCollection[F, InvestmentRankingEntity],
    private val inputs: MongoCollection[F, InvestmentRankingInputEntity]
)(using
    F: MonadThrow[F]
) extends InvestmentScreenRepository[F] {
  import InvestmentScreenRepository.Field

  override def findWithRanking(id: InvestmentScreenId): F[(InvestmentScreen, InvestmentRanking)] =
    collection
      .aggregate[InvestmentScreenWithRankingEntity](
        Aggregate
          .matchBy(Filter.idEq(id.value))
          .replaceWith(Document(Field.Screen := "$$ROOT"))
          .lookup(
            InvestmentRankingRepository.CollectionName,
            s"${Field.Screen}.${Field.RankingId}",
            InvestmentRankingRepository.Field.Id,
            Field.Rankings
          )
      )
      .first
      .flatMap(
        _.flatMap(row => row.rankings.headOption.map(ranking => (row.screen.toDomain, ranking.toDomain)))
          .liftTo[F](AppError.EntityDoesNotExist("Investment screen", id.value))
      )

  override def latestRanking: F[Option[InvestmentRanking]] =
    InvestmentRankingQueries.latest(rankings)

  override def rankingEntries(id: InvestmentRankingId): F[List[InvestmentRankingEntry]] =
    inputs
      .find(Filter.eq(InvestmentRankingRepository.Field.RankingId, id.value))
      .sort(Sort.asc(InvestmentRankingRepository.Field.Ordinal))
      .all
      .mapList(row => InvestmentRankingEntry(row.input, row.scorecard))

  override def save(screen: InvestmentScreen): F[Unit] =
    collection.insertOne(InvestmentScreenEntity.from(screen)).void

  override def get(id: InvestmentScreenId): F[Option[InvestmentScreen]] =
    collection.find(Filter.idEq(id.value)).first.mapOpt(_.toDomain)

  override def list(limit: Int): F[List[InvestmentScreenSummary]] =
    collection.find.sort(Sort.desc(Field.CreatedAt)).limit(limit).all.mapList(_.summary)

}

object InvestmentScreenRepository:
  val CollectionName = "investment-screens"

  object Field:
    val CreatedAt = "createdAt"
    val RankingId = "rankingId"
    val Screen    = "screen"
    val Rankings  = "rankings"

  def make[F[_]: MonadThrow](db: MongoDatabase[F]): F[InvestmentScreenRepository[F]] =
    for
      rawCollection <- db.getCollectionWithCodec[InvestmentScreenEntity](CollectionName)
      collection = rawCollection.withAddedCodec[InvestmentScreenWithRankingEntity]
      rankings <- db.getCollectionWithCodec[InvestmentRankingEntity](InvestmentRankingRepository.CollectionName)
      inputs   <- db.getCollectionWithCodec[InvestmentRankingInputEntity](InvestmentRankingRepository.InputsCollectionName)
      _        <- collection.createIndex(Index.descending(Field.CreatedAt))
      _        <- collection.createIndex(Index.ascending(Field.RankingId))
    yield LiveInvestmentScreenRepository[F](collection, rankings, inputs)
