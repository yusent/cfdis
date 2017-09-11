module CFDI.ParserSpec (spec) where

import CFDI
import CFDI.Parser
import Data.Maybe          (fromJust, isJust, isNothing)
import Data.Time.Calendar  (Day(ModifiedJulianDay))
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Test.Hspec

spec :: Spec
spec = do
  describe "CFDI.Parser.parseCFDI" $ do
    context "when parsing an invoice" $ do
      Just invoiceCFDI <- parseCFDI <$> runIO (readFile "test/xml/invoice.xml")

      it "parses account number" $ do
        accountNumber invoiceCFDI `shouldBe` Just "1212"

      it "parses total" $ do
        total invoiceCFDI `shouldBe` 536.216

      it "parses certificate" $ do
        certificate invoiceCFDI `shouldBe` "SOMENOTSORANDOMCERTIFICATE"

      it "parses certificate number" $ do
        certificateNumber invoiceCFDI `shouldBe` "00001000001212121212"

      it "parses complement" $ do
        let _complement = complement invoiceCFDI
        _complement `shouldSatisfy` isJust

        let maybePacStamp = pacStamp $ fromJust _complement
        maybePacStamp `shouldSatisfy` isJust

        let _pacStamp = fromJust maybePacStamp
        cfdSignature _pacStamp `shouldBe` "Signature"
        satCertificateNumber _pacStamp `shouldBe` "SAT Certificate Number"
        satSignature _pacStamp `shouldBe` "SAT Signature"
        localDay (stampedAt _pacStamp) `shouldBe` ModifiedJulianDay 57349
        localTimeOfDay (stampedAt _pacStamp) `shouldBe` TimeOfDay 23 5 0
        stampVersion _pacStamp `shouldBe` "1.0"
        uuid _pacStamp `shouldBe` "12121212-1212-1212-1212-121212121212"

      it "parses concepts" $ do
        let _concepts = concepts invoiceCFDI
        length _concepts `shouldBe` 2

        let (concept0 : concept1 : _) = _concepts
        amount concept0 `shouldBe` 112
        description concept0 `shouldBe` "Product 1 Description"
        _id concept0 `shouldBe` Just "Product 1 ID"
        let _importInfo = importInfo concept0
        length _importInfo `shouldBe` 1
        let __importInfo = head _importInfo
        custom __importInfo `shouldBe` Just "Product 1 Custom"
        importIssuedAt __importInfo `shouldBe` ModifiedJulianDay 46664
        importNumber __importInfo `shouldBe` "Product 1 Import Number"
        quantity concept0 `shouldBe` 1
        unit concept0 `shouldBe` "Product 1 Unit"
        unitAmount concept0 `shouldBe` 112

        amount concept1 `shouldBe` 424.24
        description concept1 `shouldBe` "Product 2 Description"
        _id concept1 `shouldBe` Just "Product 2 ID"
        let _importInfo = importInfo concept1
        length _importInfo `shouldBe` 0
        quantity concept1 `shouldBe` 2
        unit concept1 `shouldBe` "Product 2 Unit"
        unitAmount concept1 `shouldBe` 212.12

      it "parses currency" $ do
        currency invoiceCFDI `shouldBe` Just "MXN"

      it "parses internal ID" $ do
        internalID invoiceCFDI `shouldBe` Just "144144"

      it "parses issued date" $ do
        localDay (issuedAt invoiceCFDI) `shouldBe` ModifiedJulianDay 57916
        localTimeOfDay (issuedAt invoiceCFDI) `shouldBe` TimeOfDay 12 0 0

      it "parses where it was issued in" $ do
        issuedIn invoiceCFDI `shouldBe` "Issued In"

      it "parses issuer" $ do
        let _issuer = issuer invoiceCFDI
        let maybeFiscalAddress = fiscalAddress _issuer
        maybeFiscalAddress `shouldSatisfy` isJust
        let _fiscalAddress = fromJust maybeFiscalAddress
        fiscalCountry _fiscalAddress `shouldBe` "Fiscal Country"
        fiscalExternalNumber _fiscalAddress `shouldBe` Just "Fiscal External Number"
        fiscalInternalNumber _fiscalAddress `shouldBe` Nothing
        fiscalLocality _fiscalAddress `shouldBe` Nothing
        fiscalMunicipality _fiscalAddress `shouldBe` "Fiscal Municipality"
        fiscalReference _fiscalAddress `shouldBe` Nothing
        fiscalState _fiscalAddress `shouldBe` "Fiscal State"
        fiscalStreet _fiscalAddress `shouldBe` "Fiscal Street"
        fiscalSuburb _fiscalAddress `shouldBe` Just "Fiscal Suburb"
        fiscalZipCode _fiscalAddress `shouldBe` "Fiscal Zip Code"
        issuedInAddress _issuer `shouldSatisfy` isNothing
        name _issuer `shouldBe` Just "Issuer Name"
        regimes _issuer `shouldBe` [TaxRegime "Fiscal Regime"]
        rfc _issuer `shouldBe` "XAXX010101000"
