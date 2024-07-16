import sbt.*

object Dependencies {
  private object Versions {
    val mongo4cats  = "0.7.8"
    val commonScala = "0.1.17"
    val pureConfig  = "0.17.7"
    val circe       = "0.14.7"
    val sttp        = "3.9.7"
    val http4s      = "0.23.27"
    val logback     = "1.5.6"
    val log4cats    = "2.6.0"
  }

  private object Libraries {
    object commonScala {
      val cats     = "io.github.kirill5k" %% "common-cats"      % Versions.commonScala
      val syntax   = "io.github.kirill5k" %% "common-syntax"    % Versions.commonScala
      val testSttp = "io.github.kirill5k" %% "common-sttp-test" % Versions.commonScala
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

      val all = Seq(core, generic, parser)
    }

    object sttp {
      val core        = "com.softwaremill.sttp.client3" %% "core"  % Versions.sttp
      val circe       = "com.softwaremill.sttp.client3" %% "circe" % Versions.sttp
      val catsBackend = "com.softwaremill.sttp.client3" %% "fs2"   % Versions.sttp

      val all = Seq(core, circe, catsBackend)
    }
  }

  val core = Seq(
    Libraries.mongo4cats.core,
    Libraries.mongo4cats.circe,
    Libraries.commonScala.cats,
    Libraries.commonScala.syntax,
    Libraries.pureconfig.core
  ) ++
    Libraries.circe.all ++
    Libraries.logging.all ++
    Libraries.sttp.all

  val test = Seq(
    Libraries.commonScala.testSttp % Test,
    Libraries.mongo4cats.embedded  % Test
  )
}
