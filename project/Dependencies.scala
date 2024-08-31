import sbt.*

object Dependencies {
  private object Versions {
    val mongo4cats  = "0.7.8"
    val commonScala = "0.1.19"
    val pureConfig  = "0.17.7"
    val circe       = "0.14.9"
    val circeFs2    = "0.14.1"
    val sttp        = "3.9.8"
    val logback     = "1.5.7"
    val log4cats    = "2.7.0"
    val tapir       = "1.11.1"
    val cronUtils   = "9.2.1"
    val http4s      = "0.23.16"
  }

  private object Libraries {
    val cronUtils   = "com.cronutils" % "cron-utils"          % Versions.cronUtils
    val blazeClient = "org.http4s"   %% "http4s-blaze-client" % Versions.http4s

    object commonScala {
      val cats       = "io.github.kirill5k" %% "common-cats"        % Versions.commonScala
      val syntax     = "io.github.kirill5k" %% "common-syntax"      % Versions.commonScala
      val testSttp   = "io.github.kirill5k" %% "common-sttp-test"   % Versions.commonScala
      val http4s     = "io.github.kirill5k" %% "common-http4s"      % Versions.commonScala
      val testHttp4s = "io.github.kirill5k" %% "common-http4s-test" % Versions.commonScala
    }

    object mongo4cats {
      val core     = "io.github.kirill5k" %% "mongo4cats-core"     % Versions.mongo4cats
      val circe    = "io.github.kirill5k" %% "mongo4cats-circe"    % Versions.mongo4cats
      val embedded = "io.github.kirill5k" %% "mongo4cats-embedded" % Versions.mongo4cats
    }

    object pureconfig {
      val core = "com.github.pureconfig" %% "pureconfig-core" % Versions.pureConfig
    }

    object logging {
      val logback  = "ch.qos.logback" % "logback-classic" % Versions.logback
      val log4cats = "org.typelevel" %% "log4cats-slf4j"  % Versions.log4cats

      val all = Seq(log4cats, logback)
    }

    object circe {
      val core    = "io.circe" %% "circe-core"    % Versions.circe
      val generic = "io.circe" %% "circe-generic" % Versions.circe
      val parser  = "io.circe" %% "circe-parser"  % Versions.circe
      val fs2     = "io.circe" %% "circe-fs2"     % Versions.circeFs2

      val all = Seq(core, generic, parser, fs2)
    }

    object sttp {
      val core          = "com.softwaremill.sttp.client3" %% "core"           % Versions.sttp
      val circe         = "com.softwaremill.sttp.client3" %% "circe"          % Versions.sttp
      val catsBackend   = "com.softwaremill.sttp.client3" %% "fs2"            % Versions.sttp
      val http4sBackend = "com.softwaremill.sttp.client3" %% "http4s-backend" % Versions.sttp

      val all = Seq(core, circe, catsBackend, http4sBackend)
    }

    object tapir {
      val core   = "com.softwaremill.sttp.tapir" %% "tapir-core"          % Versions.tapir
      val circe  = "com.softwaremill.sttp.tapir" %% "tapir-json-circe"    % Versions.tapir
      val http4s = "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % Versions.tapir

      val all = Seq(core, circe, http4s)
    }
  }

  val core = Seq(
    Libraries.cronUtils,
    Libraries.blazeClient,
    Libraries.mongo4cats.core,
    Libraries.mongo4cats.circe,
    Libraries.commonScala.cats,
    Libraries.commonScala.syntax,
    Libraries.pureconfig.core,
    Libraries.commonScala.http4s
  ) ++
    Libraries.circe.all ++
    Libraries.logging.all ++
    Libraries.sttp.all ++
    Libraries.tapir.all

  val test = Seq(
    Libraries.commonScala.testHttp4s % Test,
    Libraries.commonScala.testSttp   % Test,
    Libraries.mongo4cats.embedded    % Test
  )
}
