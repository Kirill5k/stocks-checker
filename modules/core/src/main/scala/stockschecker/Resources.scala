package stockschecker

import cats.effect.{Async, Resource}
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase
import mongo4cats.models.client.{ConnectionString, MongoClientSettings}
import stockschecker.common.config.{AppConfig, MongoConfig}
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.{BackendOptions, WebSocketStreamBackend}
import sttp.client4.httpclient.fs2.HttpClientFs2Backend as Fs2Backend

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*

trait Resources[F[_]]:
  def httpBackend: WebSocketStreamBackend[F, Fs2Streams[F]]
  def mongoDatabase: MongoDatabase[F]

object Resources {

  private def fs2Backend[F[_]: Async](timeout: FiniteDuration): Resource[F, WebSocketStreamBackend[F, Fs2Streams[F]]] =
    Fs2Backend.resource[F](options = BackendOptions(timeout, None))

  private def mongoConnectionUri(c: MongoConfig): Either[Throwable, String] =
    val blank = List("user" -> c.user, "password" -> c.password, "host" -> c.host)
      .collect { case (name, value) if value.isBlank => name }
    Either.cond(
      blank.isEmpty,
      s"mongodb+srv://${c.user}:${c.password}@${c.host}/${c.dbName}",
      new IllegalArgumentException(
        s"MongoDB config is missing required fields: ${blank.mkString(", ")}. " +
          "Please set the MONGO_USER, MONGO_PASSWORD, and MONGO_HOST environment variables."
      )
    )

  private def mkMongoDatabase[F[_]: Async](config: MongoConfig): Resource[F, MongoDatabase[F]] =
    Resource
      .eval(Async[F].fromEither(mongoConnectionUri(config)))
      .flatMap { uri =>
        val settings = MongoClientSettings
          .builder()
          .retryReads(true)
          .retryWrites(true)
          .applyConnectionString(ConnectionString(uri))
          .applyToSocketSettings { builder =>
            val _ = builder
              .connectTimeout(config.connectTimeout.toMillis, TimeUnit.MILLISECONDS)
              .readTimeout(config.readTimeout.toMillis, TimeUnit.MILLISECONDS)
          }
          .applyToClusterSettings { builder =>
            val _ = builder.serverSelectionTimeout(config.serverSelectionTimeout.toMillis, TimeUnit.MILLISECONDS)
          }
          .build()
        MongoClient.create[F](settings).evalMap(_.getDatabase(config.dbName))
      }

  def make[F[_]](config: AppConfig)(using F: Async[F]): Resource[F, Resources[F]] =
    for
      hb <- fs2Backend(10.minutes)
      md <- mkMongoDatabase(config.mongo)
    yield new Resources[F]:
      def httpBackend: WebSocketStreamBackend[F, Fs2Streams[F]] = hb
      def mongoDatabase: MongoDatabase[F]                       = md
}
