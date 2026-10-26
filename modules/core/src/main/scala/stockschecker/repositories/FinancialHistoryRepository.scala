package stockschecker.repositories

import cats.effect.Concurrent
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Accumulator, Aggregate, Filter, Index, Sort}
import stockschecker.domain.{FinancialSnapshot, Ticker}

trait FinancialHistoryRepository[F[_]]:
  def save(snapshot: FinancialSnapshot): F[Unit]
  def findLatest(ticker: Ticker): F[Option[FinancialSnapshot]]
  def findLatestAll: F[List[FinancialSnapshot]]

private final class LiveFinancialHistoryRepository[F[_]: Concurrent](collection: MongoCollection[F, FinancialSnapshot])
    extends FinancialHistoryRepository[F]:
  override def save(snapshot: FinancialSnapshot): F[Unit] = collection.insertOne(snapshot).void

  override def findLatest(ticker: Ticker): F[Option[FinancialSnapshot]] =
    collection.find(Filter.eq("ticker", ticker.value)).sort(Sort.desc("observedAt")).first

  override def findLatestAll: F[List[FinancialSnapshot]] =
    collection.aggregate[FinancialSnapshot](Aggregate.sort(Sort.desc("observedAt"))
      .group("$ticker", Accumulator.first("snapshot", "$$ROOT"))
      .replaceWith("$snapshot").sort(Sort.asc("ticker"))).all.map(_.toList)

object FinancialHistoryRepository extends MongoJsonCodecs:
  val CollectionName = "financial-history"

  def make[F[_]: Concurrent](database: MongoDatabase[F]): F[FinancialHistoryRepository[F]] =
    for
      collection <- database.getCollectionWithCodec[FinancialSnapshot](CollectionName)
      _          <- collection.createIndex(Index.ascending("ticker"))
      _          <- collection.createIndex(Index.descending("observedAt"))
    yield LiveFinancialHistoryRepository[F](collection)
