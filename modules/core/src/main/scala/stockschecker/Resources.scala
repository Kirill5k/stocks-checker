package stockschecker

import cats.effect.{Async, Resource}
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase
import mongo4cats.models.client.{ConnectionString, MongoClientSettings}
import stockschecker.common.config.{AppConfig, MongoConfig}
import sttp.capabilities.WebSockets
import sttp.capabilities.fs2.Fs2Streams
import sttp.client3.httpclient.fs2.HttpClientFs2Backend
import sttp.client3.{SttpBackend, SttpBackendOptions}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*

trait Resources[F[_]]:
  def httpBackend: SttpBackend[F, Fs2Streams[F] & WebSockets]
  def mongoDatabase: MongoDatabase[F]

object Resources {

  private def mkHttpClientBackend[F[_]: Async](timeout: FiniteDuration): Resource[F, SttpBackend[F, Fs2Streams[F] & WebSockets]] =
    HttpClientFs2Backend.resource[F](SttpBackendOptions(connectionTimeout = timeout, proxy = None))

  private def mkMongoDatabase[F[_]: Async](config: MongoConfig): Resource[F, MongoDatabase[F]] =
    val settings = MongoClientSettings
      .builder()
      .applyConnectionString(ConnectionString(config.connectionUri))
      .applyToSocketSettings { builder =>
        val _ = builder.connectTimeout(3, TimeUnit.MINUTES).readTimeout(3, TimeUnit.MINUTES)
      }
      .applyToClusterSettings { builder =>
        val _ = builder.serverSelectionTimeout(3, TimeUnit.MINUTES)
      }
      .build()
    MongoClient.create[F](settings).evalMap(_.getDatabase(config.dbName))

  def make[F[_]](config: AppConfig)(using F: Async[F]): Resource[F, Resources[F]] =
    for
      hb <- mkHttpClientBackend(1.minute)
      md <- mkMongoDatabase(config.mongo)
    yield new Resources[F]:
      def httpBackend: SttpBackend[F, Fs2Streams[F] & WebSockets] = hb
      def mongoDatabase: MongoDatabase[F]  = md
}
