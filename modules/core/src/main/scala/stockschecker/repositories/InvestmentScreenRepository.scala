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
import mongo4cats.operations.{Aggregate, Filter, Index, Projection, Sort}
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.repositories.entities.{
  Entity,
  InvestmentRankingEntity,
  InvestmentRankingInputEntity,
  InvestmentScreenEntity,
  InvestmentScreenWithRankingEntity
}

trait InvestmentScreenRepository[F[_]]:
  def findWithRanking(id: InvestmentScreenId): F[(InvestmentScreen, InvestmentRanking)]
  def latestRanking: F[Option[InvestmentRanking]]
  def rankingEntries(id: InvestmentRankingId): F[List[InvestmentRankingEntry]]
  def detailEntries(screen: InvestmentScreen, includeExclusions: Boolean): F[List[InvestmentRankingEntry]]
  def visibleRankingTickers(id: InvestmentRankingId, tickers: Option[Set[Ticker]] = None): F[Set[Ticker]]
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

  private def inputIds(id: InvestmentRankingId, tickers: Iterable[Ticker]): List[String] =
    tickers.iterator.map(ticker => s"${id.value}:${ticker.value}").toList

  override def detailEntries(screen: InvestmentScreen, includeExclusions: Boolean): F[List[InvestmentRankingEntry]] =
    if includeExclusions then rankingEntries(screen.rankingId)
    else
      val holdingIds     = inputIds(screen.rankingId, screen.cohorts.flatMap(_.tickers).toSet)
      val nearMissFilter = List(
        Some(Filter.eq(InvestmentRankingRepository.Field.RankingId, screen.rankingId.value)),
        Some(Filter.eq(Field.ScorecardEligible, false)),
        Some(Filter.isNull(Field.ExclusionExplanation).not),
        screen.request.exchange.map(exchange => Filter.eq(Field.InputExchange, exchange.print)),
        screen.request.country.map(country => Filter.eq(Field.InputCountry, country))
      ).flatten.foldLeft(Filter.empty)(_ && _)
      for
        holdings   <- inputs.find(Filter.in(InvestmentRankingRepository.Field.Id, holdingIds)).all
        nearMisses <- inputs
          .aggregate[InvestmentRankingInputEntity](
            Aggregate
              .matchBy(nearMissFilter)
              .sort(Sort.asc(InvestmentRankingRepository.Field.Ordinal))
              .combinedWith(StockVisibility.restrict(Field.ScorecardTicker))
              .limit(InvestmentRecipe.NearMissLimit)
          )
          .all
      yield (holdings ++ nearMisses).toList
        .distinctBy(_.scorecard.ticker)
        .sortBy(_.ordinal)
        .map(row => InvestmentRankingEntry(row.input, row.scorecard))

  override def visibleRankingTickers(id: InvestmentRankingId, tickers: Option[Set[Ticker]]): F[Set[Ticker]] =
    val rankingFilter = Filter.eq(InvestmentRankingRepository.Field.RankingId, id.value)
    val filter        =
      tickers.fold(rankingFilter)(values => rankingFilter && Filter.in(InvestmentRankingRepository.Field.Id, inputIds(id, values)))
    inputs
      .aggregate[Entity](
        Aggregate
          .matchBy(filter)
          .project(Projection.computed(InvestmentRankingRepository.Field.Id, s"$$${Field.ScorecardTicker}"))
          .combinedWith(StockVisibility.restrict(InvestmentRankingRepository.Field.Id))
      )
      .all
      .map(_.iterator.map(_._id).toSet)

  override def save(screen: InvestmentScreen): F[Unit] =
    collection.insertOne(InvestmentScreenEntity.from(screen)).void

  override def get(id: InvestmentScreenId): F[Option[InvestmentScreen]] =
    collection.find(Filter.idEq(id.value)).first.mapOpt(_.toDomain)

  override def list(limit: Int): F[List[InvestmentScreenSummary]] =
    collection
      .aggregate[InvestmentScreenEntity](InvestmentScreenVisibility.restrict.sort(Sort.desc(Field.CreatedAt)).limit(limit))
      .all
      .mapList(_.summary)

}

object InvestmentScreenRepository:
  val CollectionName = "investment-screens"

  object Field:
    val CreatedAt            = "createdAt"
    val RankingId            = "rankingId"
    val Screen               = "screen"
    val Rankings             = "rankings"
    val ScorecardTicker      = "scorecard.ticker"
    val ScorecardEligible    = "scorecard.eligible"
    val InputExchange        = "input.exchange"
    val InputCountry         = "input.country"
    val ExclusionExplanation = "input.exclusionExplanation"

  def make[F[_]: MonadThrow](db: MongoDatabase[F]): F[InvestmentScreenRepository[F]] =
    for
      rawCollection <- db.getCollectionWithCodec[InvestmentScreenEntity](CollectionName)
      collection = rawCollection.withAddedCodec[InvestmentScreenWithRankingEntity]
      rankings <- db.getCollectionWithCodec[InvestmentRankingEntity](InvestmentRankingRepository.CollectionName)
      inputs   <- db.getCollectionWithCodec[InvestmentRankingInputEntity](InvestmentRankingRepository.InputsCollectionName)
      _        <- collection.createIndex(Index.descending(Field.CreatedAt))
      _        <- collection.createIndex(Index.ascending(Field.RankingId))
    yield LiveInvestmentScreenRepository[F](collection, rankings, inputs.withAddedCodec[Entity])
