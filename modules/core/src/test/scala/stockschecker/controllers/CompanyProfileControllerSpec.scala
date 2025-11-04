package stockschecker.controllers

import cats.effect.IO
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.http4s.*
import org.http4s.implicits.*
import stockschecker.domain.Ticker
import stockschecker.domain.errors.AppError
import stockschecker.services.CompanyProfileService
import stockschecker.fixtures.*

class CompanyProfileControllerSpec extends HttpRoutesWordSpec {

  "A CompanyProfileController" when {
    "GET /company-profiles/:ticker" should {
      "return 200 and company profile on success" in {
        val svc = mocks
        when(svc.get(any[Ticker], anyBoolean)).thenReturnIO(AAPLCompanyProfile)

        val res = for
          controller <- CompanyProfileController.make(svc)
          req = Request[IO](uri = uri"/company-profiles/AAPL?fetchLatest=true", method = Method.GET)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody = s"""{
                              |  "ticker" : "AAPL",
                              |  "name" : "Apple Inc.",
                              |  "country" : "US",
                              |  "industry" : "Consumer Electronics",
                              |  "description" : "Apple Inc. designs, manufactures, and markets smartphones, personal computers, tablets, wearables, and accessories worldwide. The company offers iPhone, a line of smartphones; Mac, a line of personal computers; iPad, a line of multi-purpose tablets; and wearables, home, and accessories comprising AirPods, Apple TV, Apple Watch, Beats products, and HomePod. It also provides AppleCare support and cloud services; and operates various platforms, including the App Store that allow customers to discover and download applications and digital content, such as books, music, video, games, and podcasts. In addition, the company offers various services, such as Apple Arcade, a game subscription service; Apple Fitness+, a personalized fitness service; Apple Music, which offers users a curated listening experience with on-demand radio stations; Apple News+, a subscription news and magazine service; Apple TV+, which offers exclusive original content; Apple Card, a co-branded credit card; and Apple Pay, a cashless payment service, as well as licenses its intellectual property. The company serves consumers, and small and mid-sized businesses; and the education, enterprise, and government markets. It distributes third-party applications for its products through the App Store. The company also sells its products through its retail and online stores, and direct sales force; and third-party cellular network carriers, wholesalers, retailers, and resellers. Apple Inc. was incorporated in 1977 and is headquartered in Cupertino, California.",
                              |  "website" : "https://www.apple.com",
                              |  "ipoDate" : "1980-12-12",
                              |  "currency" : "USD",
                              |  "marketCap" : 3439591971000,
                              |  "pricePerformanceLastUpdatedAt" : null
                              |}""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).get(AAPL, true)
      }

      "return 404 on not found" in {
        val svc = mocks
        when(svc.get(any[Ticker], anyBoolean)).thenRaiseError(AppError.CompanyProfileNotFound(AAPL))

        val res = for
          controller <- CompanyProfileController.make(svc)
          req = Request[IO](uri = uri"/company-profiles/AAPL", method = Method.GET)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.NotFound, Some("""{"message":"Could not find company profile for AAPL"}"""))
        verify(svc).get(AAPL, false)
      }
    }

    "GET /company-profiles" should {
      "return 200 and list of company profiles on success" in {
        val svc = mocks
        when(svc.getAll(any[Option[Int]])).thenReturnIO(List(AAPLCompanyProfile, MSFTCompanyProfile))

        val res = for
          controller <- CompanyProfileController.make(svc)
          req = Request[IO](uri = uri"/company-profiles", method = Method.GET)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody = s"""[{
                              |  "ticker" : "AAPL",
                              |  "name" : "Apple Inc.",
                              |  "country" : "US",
                              |  "industry" : "Consumer Electronics",
                              |  "description" : "Apple Inc. designs, manufactures, and markets smartphones, personal computers, tablets, wearables, and accessories worldwide. The company offers iPhone, a line of smartphones; Mac, a line of personal computers; iPad, a line of multi-purpose tablets; and wearables, home, and accessories comprising AirPods, Apple TV, Apple Watch, Beats products, and HomePod. It also provides AppleCare support and cloud services; and operates various platforms, including the App Store that allow customers to discover and download applications and digital content, such as books, music, video, games, and podcasts. In addition, the company offers various services, such as Apple Arcade, a game subscription service; Apple Fitness+, a personalized fitness service; Apple Music, which offers users a curated listening experience with on-demand radio stations; Apple News+, a subscription news and magazine service; Apple TV+, which offers exclusive original content; Apple Card, a co-branded credit card; and Apple Pay, a cashless payment service, as well as licenses its intellectual property. The company serves consumers, and small and mid-sized businesses; and the education, enterprise, and government markets. It distributes third-party applications for its products through the App Store. The company also sells its products through its retail and online stores, and direct sales force; and third-party cellular network carriers, wholesalers, retailers, and resellers. Apple Inc. was incorporated in 1977 and is headquartered in Cupertino, California.",
                              |  "website" : "https://www.apple.com",
                              |  "ipoDate" : "1980-12-12",
                              |  "currency" : "USD",
                              |  "marketCap" : 3439591971000,
                              |  "pricePerformanceLastUpdatedAt" : null
                              |}, {
                              |  "ticker" : "MSFT",
                              |  "name" : "Microsoft Corporation",
                              |  "country" : "US",
                              |  "industry" : "Software—Infrastructure",
                              |  "description" : "Microsoft Corporation develops, licenses, and supports software, services, devices, and solutions worldwide.",
                              |  "website" : "https://www.microsoft.com",
                              |  "ipoDate" : "1986-03-13",
                              |  "currency" : "USD",
                              |  "marketCap" : 3100000000000,
                              |  "pricePerformanceLastUpdatedAt" : null
                              |}]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).getAll(None)
      }

      "return 200 and limited list of company profiles with limit parameter" in {
        val svc = mocks
        when(svc.getAll(any[Option[Int]])).thenReturnIO(List(AAPLCompanyProfile))

        val res = for
          controller <- CompanyProfileController.make(svc)
          req = Request[IO](uri = uri"/company-profiles?limit=1", method = Method.GET)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody = s"""[{
                              |  "ticker" : "AAPL",
                              |  "name" : "Apple Inc.",
                              |  "country" : "US",
                              |  "industry" : "Consumer Electronics",
                              |  "description" : "Apple Inc. designs, manufactures, and markets smartphones, personal computers, tablets, wearables, and accessories worldwide. The company offers iPhone, a line of smartphones; Mac, a line of personal computers; iPad, a line of multi-purpose tablets; and wearables, home, and accessories comprising AirPods, Apple TV, Apple Watch, Beats products, and HomePod. It also provides AppleCare support and cloud services; and operates various platforms, including the App Store that allow customers to discover and download applications and digital content, such as books, music, video, games, and podcasts. In addition, the company offers various services, such as Apple Arcade, a game subscription service; Apple Fitness+, a personalized fitness service; Apple Music, which offers users a curated listening experience with on-demand radio stations; Apple News+, a subscription news and magazine service; Apple TV+, which offers exclusive original content; Apple Card, a co-branded credit card; and Apple Pay, a cashless payment service, as well as licenses its intellectual property. The company serves consumers, and small and mid-sized businesses; and the education, enterprise, and government markets. It distributes third-party applications for its products through the App Store. The company also sells its products through its retail and online stores, and direct sales force; and third-party cellular network carriers, wholesalers, retailers, and resellers. Apple Inc. was incorporated in 1977 and is headquartered in Cupertino, California.",
                              |  "website" : "https://www.apple.com",
                              |  "ipoDate" : "1980-12-12",
                              |  "currency" : "USD",
                              |  "marketCap" : 3439591971000,
                              |  "pricePerformanceLastUpdatedAt" : null
                              |}]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).getAll(Some(1))
      }

      "return 200 and empty list when no company profiles exist" in {
        val svc = mocks
        when(svc.getAll(any[Option[Int]])).thenReturnIO(List.empty)

        val res = for
          controller <- CompanyProfileController.make(svc)
          req = Request[IO](uri = uri"/company-profiles", method = Method.GET)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Ok, Some("[]"))
        verify(svc).getAll(None)
      }
    }
  }

  def mocks: CompanyProfileService[IO] = mock[CompanyProfileService[IO]]
}
