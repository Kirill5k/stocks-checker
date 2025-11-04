package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{Exchange, Security}
import stockschecker.repositories.SecurityRepository
import stockschecker.fixtures.*

class SecurityServiceSpec extends IOWordSpec {
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  "A SecurityService" when {
    "fetchLatestSecurities" should {
      "fetch traded securities from client and store them in db" in {
        val (repo, client) = mocks
        when(client.getTradedSecurities(any[Exchange])).thenStream(AAPLSecurity)
        when(repo.save(anyList[Security])).thenReturnUnit
        val res = for
          svc <- SecurityService.make(repo, client)
          _   <- svc.fetchLatest(NonEmptyList.of(Exchange.NASDAQ))
        yield ()

        res.asserting { r =>
          verify(client).getTradedSecurities(Exchange.NASDAQ)
          verify(repo).save(List(AAPLSecurity))
          r mustBe ()
        }
      }
    }
  }

  def mocks: (SecurityRepository[IO], MarketDataClient[IO]) =
    (mock[SecurityRepository[IO]], mock[MarketDataClient[IO]])
}
