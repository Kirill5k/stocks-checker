package stockschecker.services

import cats.effect.IO
import stockschecker.domain.*

/** Isolates service tests from availability storage; observation behavior has its own integration coverage. */
object DataQualityServiceStub extends DataQualityService[IO]:
  override def observe[A](ticker: Ticker, provider: Provider, dataset: String)(request: IO[A])(present: A => Boolean): IO[A] = request
  override def report: IO[DataQualityReport] = IO.raiseError(new UnsupportedOperationException("Report is outside this test"))
