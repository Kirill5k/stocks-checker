package stockschecker.clients.finnhub

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import stockschecker.domain.Sector

class FinnhubSectorMappingSpec extends AnyWordSpec with Matchers:
  "FinnhubSectorMapping" should {
    "normalize provider industry names while retaining typed sectors" in {
      FinnhubSectorMapping.sectorFor("  SEMICONDUCTORS  ") mustBe Some(Sector.Technology)
      FinnhubSectorMapping.sectorFor("Health Care") mustBe Some(Sector.Healthcare)
      FinnhubSectorMapping.sectorFor("Retail") mustBe Some(Sector.ConsumerDiscretionary)
      FinnhubSectorMapping.sectorFor("Banks") mustBe Some(Sector.Financials)
      FinnhubSectorMapping.sectorFor("REIT") mustBe Some(Sector.RealEstate)
    }

    "leave unknown provider industries unmapped" in {
      FinnhubSectorMapping.sectorFor("unknown provider industry") mustBe None
      FinnhubSectorMapping.sectorFor("") mustBe None
    }
  }
