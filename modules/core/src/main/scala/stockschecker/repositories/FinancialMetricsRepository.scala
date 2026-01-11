package stockschecker.repositories

import cats.Monad
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import kirill5k.common.cats.Clock
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Filter, Index, Update}
import mongo4cats.circe.MongoJsonCodecs
import stockschecker.domain.{FinancialMetrics, Ticker}
import stockschecker.repositories.entities.FinancialMetricsEntity
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.models.collection.{UpdateOptions, WriteCommand}

import java.time.Instant

trait FinancialMetricsRepository[F[_]]:
  def save(fm: List[FinancialMetrics]): F[Unit]
  def find(ticker: Ticker): F[Option[FinancialMetrics]]

final private class LiveFinancialMetricsRepository[F[_]](
    private val collection: MongoCollection[F, FinancialMetricsEntity]
)(using
    M: Monad[F],
    C: Clock[F]
) extends FinancialMetricsRepository[F] {

  import FinancialMetricsRepository.Field

  extension (fm: FinancialMetrics)
    private def toUpdate(now: Instant): Update =
      Update
        .setOnInsert(Field.Id, fm.ticker)
        .setOnInsert(Field.CreatedAt, now)
        .set(Field.PeTTM, fm.peTTM)
        .set(Field.EpsTTM, fm.epsTTM)
        .set(Field.RoeTTM, fm.roeTTM)
        .set(Field.DividendYield, fm.dividendYieldIndicatedAnnual)
        .set(Field.DebtToEquity, fm.totalDebtToEquityAnnual)
        .set(Field.ProfitMargin, fm.netProfitMarginTTM)
        .set(Field.FcfPerShare, fm.freeCashFlowPerShareTTM)
        .set(Field.RevenueGrowth5Y, fm.revenueGrowth5Y)
        .set(Field.EpsGrowth5Y, fm.epsGrowth5Y)
        .set(Field.FiftyTwoWeekHigh, fm.fiftyTwoWeekHigh)
        .set(Field.FiftyTwoWeekLow, fm.fiftyTwoWeekLow)
        .set(Field.UpdatedAt, now)

  override def save(fms: List[FinancialMetrics]): F[Unit] =
    C.now.flatMap { now =>
      val updateOpt = UpdateOptions(upsert = true)
      val updates   = fms.map(a => WriteCommand.UpdateOne(Filter.idEq(a.ticker.value), a.toUpdate(now), updateOpt))
      collection.bulkWrite(updates).void
    }

  override def find(ticker: Ticker): F[Option[FinancialMetrics]] =
    collection.find(Filter.idEq(ticker)).first.mapOpt(_.toDomain)
}

object FinancialMetricsRepository extends MongoJsonCodecs:
  val CollectionName = "financial-metrics"
  object Field:
    val Id               = "_id"
    val PeTTM            = "peTTM"
    val EpsTTM           = "epsTTM"
    val RoeTTM           = "roeTTM"
    val DividendYield    = "dividendYieldIndicatedAnnual"
    val DebtToEquity     = "totalDebtToEquityAnnual"
    val ProfitMargin     = "netProfitMarginTTM"
    val FcfPerShare      = "freeCashFlowPerShareTTM"
    val RevenueGrowth5Y  = "revenueGrowth5Y"
    val EpsGrowth5Y      = "epsGrowth5Y"
    val FiftyTwoWeekHigh = "fiftyTwoWeekHigh"
    val FiftyTwoWeekLow  = "fiftyTwoWeekLow"
    val UpdatedAt        = "updatedAt"
    val CreatedAt        = "createdAt"

  def make[F[_]: {Monad, Clock}](database: MongoDatabase[F]): F[FinancialMetricsRepository[F]] =
    for
      collection <- database.getCollectionWithCodec[FinancialMetricsEntity](CollectionName)
      _          <- collection.createIndex(Index.ascending(Field.UpdatedAt))
    yield LiveFinancialMetricsRepository[F](collection.withAddedCodec[Ticker])
