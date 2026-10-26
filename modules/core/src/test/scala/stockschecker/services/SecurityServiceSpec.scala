package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.IO
import fs2.Stream
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
        when(repo.reconcile(any[Exchange], anyList[Security])).thenReturnUnit
        val res = for
          svc <- SecurityService.make(repo, client)
          _   <- svc.fetchLatest(NonEmptyList.of(Exchange.NASDAQ))
        yield ()

        res.asserting { r =>
          verify(client).getTradedSecurities(Exchange.NASDAQ)
          verify(repo).reconcile(Exchange.NASDAQ, List(AAPLSecurity))
          r mustBe ()
        }
      }

      "leave listing state unchanged when a provider stream fails partway through" in {
        val (repo, client) = mocks
        val error          = new RuntimeException("Truncated listing response")
        when(client.getTradedSecurities(Exchange.NASDAQ)).thenReturn(
          Stream.emit(AAPLSecurity).covary[IO] ++ Stream.raiseError[IO](error)
        )
        val result = for
          svc <- SecurityService.make(repo, client)
          _   <- svc.fetchLatest(NonEmptyList.of(Exchange.NASDAQ))
        yield ()
        result.attempt.asserting { outcome =>
          outcome mustBe Left(error)
          verifyNoInteractions(repo)
          succeed
        }
      }

      "refuse to reconcile an unexpectedly empty exchange listing" in {
        val (repo, client) = mocks
        when(client.getTradedSecurities(Exchange.NASDAQ)).thenReturn(Stream.empty)
        val result = for
          svc <- SecurityService.make(repo, client)
          _   <- svc.fetchLatest(NonEmptyList.of(Exchange.NASDAQ))
        yield ()
        result.attempt.asserting { outcome =>
          outcome.isLeft mustBe true
          verifyNoInteractions(repo)
          succeed
        }
      }
    }

  }

  def mocks: (SecurityRepository[IO], MarketDataClient[IO]) =
    (mock[SecurityRepository[IO]], mock[MarketDataClient[IO]])
}
