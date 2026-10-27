package stockschecker

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

class ProviderBoundarySpec extends AnyWordSpec with Matchers {
  "Provider boundary" should {
    "keep provider cases inside clients and application configuration" in {
      val roots        = List(Path.of("src/main/scala/stockschecker"), Path.of("modules/core/src/main/scala/stockschecker"))
      val root         = roots.find(Files.isDirectory(_)).getOrElse(fail("Cannot find application sources"))
      val providerCase = "\\bProvider\\s*\\.\\s*(?:[A-Z][A-Za-z0-9_]*|\\{|\\*)".r
      val violations   = Using.resource(Files.walk(root)) { paths =>
        paths.iterator.asScala
          .filter(path => Files.isRegularFile(path) && path.toString.endsWith(".scala"))
          .filterNot { path =>
            val relative = root.relativize(path).toString
            relative.startsWith("clients/") || relative.startsWith("common/config/") || relative == "common/config.scala" ||
            relative == "Application.scala"
          }
          .flatMap { path =>
            Files.readAllLines(path).asScala.zipWithIndex.collect {
              case (line, index) if providerCase.findFirstIn(line).nonEmpty => s"${root.relativize(path)}:${index + 1}: $line"
            }
          }
          .toList
      }
      violations mustBe Nil
    }
  }
}
