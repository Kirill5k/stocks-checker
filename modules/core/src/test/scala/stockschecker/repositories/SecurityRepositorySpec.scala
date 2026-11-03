package stockschecker.repositories

import cats.data.NonEmptyList
import cats.effect.IO
import kirill5k.common.cats.Clock
import stockschecker.domain.{Exchange, ListingStatus, SecurityFilter, SecurityKind, Ticker}
import stockschecker.domain.investment.InvestmentEvaluator
import stockschecker.fixtures.*
import stockschecker.repositories.SecurityRepository.ActivationResult

import scala.concurrent.duration.*

class SecurityRepositorySpec extends RepositorySpec {
  override def port: Int = 12149
  given Clock[IO]        = Clock.mock(ts)

  "Security tombstones" should {
    "preserve exclusions through listing reconciliation and clear freshness" in
      withEmbeddedMongoDatabase { db =>
        for
          repo     <- SecurityRepository.make[IO](db)
          _        <- repo.save(List(AAPLSecurity.copy(companyProfileLastUpdatedAt = Some(ts)), MSFTSecurity))
          applied  <- repo.setActive(AAPL, false, ts, "provider-profile-empty")
          delisted <- repo.reconcile(Exchange.NASDAQ, List(MSFTSecurity))
          _        <- repo.reconcile(Exchange.NASDAQ, List(AAPLSecurity, MSFTSecurity))
          result   <- readSecurity(db, AAPL)
        yield
          applied mustBe ActivationResult.Applied
          delisted mustBe Nil
          result.map(_.isActive) mustBe Some(false)
          result.flatMap(_.exclusionReason) mustBe Some("provider-profile-empty")
          result.flatMap(_.companyProfileLastUpdatedAt) mustBe None
      }

    "retain nanosecond event ordering and accept equal-time retries" in
      withEmbeddedMongoDatabase { db =>
        for
          repo   <- SecurityRepository.make[IO](db)
          _      <- repo.save(List(AAPLSecurity))
          _      <- repo.setActive(AAPL, false, ts.plusNanos(2))
          _      <- repo.save(List(AAPLSecurity))
          stale  <- repo.setActive(AAPL, true, ts.plusNanos(1))
          replay <- repo.setActive(AAPL, false, ts.plusNanos(2))
          result <- readSecurity(db, AAPL)
        yield
          stale mustBe ActivationResult.Stale
          replay mustBe ActivationResult.Applied
          result.map(_.isActive) mustBe Some(false)
      }

    "return typed missing and delisted results without reactivating tombstones" in
      withEmbeddedMongoDatabase { db =>
        for
          repo     <- SecurityRepository.make[IO](db)
          missing  <- repo.setActive(AAPL, true, ts)
          _        <- repo.save(List(AAPLSecurity, MSFTSecurity))
          removed  <- repo.reconcile(Exchange.NASDAQ, List(MSFTSecurity))
          rejected <- repo.setActive(AAPL, true, ts)
          result   <- readSecurity(db, AAPL)
        yield
          missing mustBe ActivationResult.Missing
          removed mustBe List(AAPL)
          rejected mustBe ActivationResult.Delisted
          result.map(_.listingStatus) mustBe Some(ListingStatus.Delisted)
      }

    "delist only the requested exchange and preserve the benchmark" in
      withEmbeddedMongoDatabase { db =>
        val other     = AAPLSecurity.copy(ticker = Ticker("IBM"), exchange = Exchange.NYSE)
        val benchmark = AAPLSecurity.copy(ticker = InvestmentEvaluator.Benchmark)
        for
          repo           <- SecurityRepository.make[IO](db)
          _              <- repo.save(List(AAPLSecurity.copy(companyProfileLastUpdatedAt = Some(ts)), MSFTSecurity, other, benchmark))
          count          <- repo.countListed(Exchange.NASDAQ)
          removed        <- repo.reconcile(Exchange.NASDAQ, List(MSFTSecurity))
          missing        <- readSecurity(db, AAPL)
          retained       <- readSecurity(db, other.ticker)
          protectedStock <- readSecurity(db, benchmark.ticker)
          repeated       <- repo.reconcile(Exchange.NASDAQ, List(MSFTSecurity))
        yield
          count mustBe 3L
          removed mustBe List(AAPL)
          repeated mustBe Nil
          missing.flatMap(_.companyProfileLastUpdatedAt) mustBe None
          retained.map(_.isActive) mustBe Some(true)
          protectedStock.map(_.isActive) mustBe Some(true)
      }

    "retain delisting purge requests across retries until dispatch is acknowledged" in
      withEmbeddedMongoDatabase { db =>
        for
          repo         <- SecurityRepository.make[IO](db)
          _            <- repo.save(List(AAPLSecurity, MSFTSecurity))
          _            <- repo.reconcile(Exchange.NASDAQ, List(MSFTSecurity))
          first        <- repo.pendingPurges
          repeated     <- repo.reconcile(Exchange.NASDAQ, List(MSFTSecurity))
          pending      <- repo.pendingPurges
          _            <- repo.acknowledgePurge(AAPL, ts)
          _            <- repo.reconcile(Exchange.NASDAQ, List(MSFTSecurity))
          acknowledged <- repo.pendingPurges
        yield
          first mustBe List(SecurityRepository.PendingPurge(AAPL, ts))
          repeated mustBe Nil
          pending mustBe first
          acknowledged mustBe Nil
      }

    "preserve the latest pending purge when an earlier dispatch is acknowledged" in
      withEmbeddedMongoDatabase { db =>
        for
          repo         <- SecurityRepository.make[IO](db)
          _            <- repo.save(List(AAPLSecurity))
          _            <- repo.setActive(AAPL, false, ts.plusNanos(1))
          first        <- repo.pendingPurges
          _            <- repo.setActive(AAPL, false, ts.plusNanos(2))
          _            <- repo.acknowledgePurge(AAPL, ts.plusNanos(1))
          newer        <- repo.pendingPurges
          _            <- repo.acknowledgePurge(AAPL, ts.plusNanos(2))
          acknowledged <- repo.pendingPurges
        yield
          first mustBe List(SecurityRepository.PendingPurge(AAPL, ts.plusNanos(1)))
          newer mustBe List(SecurityRepository.PendingPurge(AAPL, ts.plusNanos(2)))
          acknowledged mustBe Nil
      }

    "make relisted non-excluded securities eligible for a profile refetch" in
      withEmbeddedMongoDatabase { db =>
        for
          repo   <- SecurityRepository.make[IO](db)
          _      <- repo.save(List(AAPLSecurity.copy(companyProfileLastUpdatedAt = Some(ts)), MSFTSecurity))
          _      <- repo.reconcile(Exchange.NASDAQ, List(MSFTSecurity))
          _      <- repo.reconcile(Exchange.NASDAQ, List(AAPLSecurity, MSFTSecurity))
          due    <- repo.findTickersBy(SecurityFilter.CompanyProfileNotUpdatedFor(3.days), None)
          result <- readSecurity(db, AAPL)
        yield
          due.toSet mustBe Set(AAPL, MSFT)
          result.map(_.isActive) mustBe Some(true)
          result.flatMap(_.companyProfileLastUpdatedAt) mustBe None
      }
  }

  "Security reads" should {
    "apply explicit activation filters without exposing inactive or delisted securities" in
      withEmbeddedMongoDatabase(
        Map(
          SecurityRepository.CollectionName -> List(
            toDoc(AAPLSecurity),
            toDoc(MSFTSecurity.copy(isActive = false)),
            toDoc(AAPLSecurity.copy(ticker = Ticker("DELISTED"), listingStatus = ListingStatus.Delisted))
          )
        )
      ) { db =>
        for
          repo     <- SecurityRepository.make[IO](db)
          active   <- repo.findTickersBy(SecurityFilter.IsActive(true), Some(1))
          inactive <- repo.findTickersBy(SecurityFilter.IsActive(false), None)
          nested   <- repo.findTickersBy(
            SecurityFilter.Composite(
              NonEmptyList.of(
                SecurityFilter.KindIs(SecurityKind.Stock),
                SecurityFilter.Composite(NonEmptyList.one(SecurityFilter.IsActive(true)))
              )
            ),
            None
          )
        yield
          active mustBe List(AAPL)
          inactive mustBe Nil
          nested mustBe List(AAPL)
      }

    "require active listed securities and profiles, but allow profile recovery without a cached profile" in
      withEmbeddedMongoDatabase { db =>
        for
          repo     <- SecurityRepository.make[IO](db)
          profiles <- CompanyProfileRepository.make[IO](db)
          _        <- repo.save(List(AAPLSecurity, MSFTSecurity))
          _        <- profiles.save(AAPLCompanyProfile)
          visible  <- repo.getAllTickers
          due      <- repo.findTickersBy(SecurityFilter.KindIs(SecurityKind.Stock), None)
          _        <- repo.setActive(AAPL, false, ts)
          hidden   <- repo.findVisible(AAPL)
        yield
          visible mustBe List(AAPL)
          due.toSet mustBe Set(AAPL, MSFT)
          hidden mustBe None
      }

    List(
      ("exchange", SecurityFilter.ExchangeIs(Exchange.NASDAQ), Set(AAPL, MSFT)),
      ("kind", SecurityFilter.KindIs(SecurityKind.ETF), Set.empty),
      ("recent", SecurityFilter.UpdatedWithin(1.day), Set(AAPL, MSFT)),
      ("stale", SecurityFilter.NotUpdatedFor(1.day), Set.empty),
      (
        "composite",
        SecurityFilter.Composite(NonEmptyList.of(SecurityFilter.ExchangeIs(Exchange.NASDAQ), SecurityFilter.KindIs(SecurityKind.Stock))),
        Set(AAPL, MSFT)
      )
    ).foreach { case (name, filter, expected) =>
      s"apply $name selection before limits" in
        withEmbeddedMongoDatabase { db =>
          for
            repo    <- SecurityRepository.make[IO](db)
            _       <- repo.save(List(AAPLSecurity, MSFTSecurity))
            result  <- repo.findTickersBy(filter, None)
            limited <- repo.findTickersBy(filter, Some(1))
          yield
            result.toSet mustBe expected
            limited.size mustBe math.min(expected.size, 1)
        }
    }
  }
}
