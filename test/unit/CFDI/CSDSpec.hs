{-# LANGUAGE OverloadedStrings #-}

module CFDI.CSDSpec (spec) where

import CFDI
import CFDI.CSD
import Data.Either         (isRight)
import Prelude      hiding (readFile)
import Data.Text           (Text)
import Data.Text.IO        (readFile)
import Data.Time.Calendar  (Day(ModifiedJulianDay))
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Test.Hspec

cfdi :: CFDI
cfdi = CFDI
  { accountNumber     = Just "1212"
  , certificate       = "SOMENOTSORANDOMCERTIFICATE"
  , certificateNumber = "00001000001212121212"
  , complement        = Just Complement
    { pacStamp = Just PacStamp
      { cfdSignature         = "Signature"
      , satCertificateNumber = "00001000002424242424"
      , satSignature         = "SAT Signature"
      , stampedAt            = LocalTime
        { localDay       = ModifiedJulianDay 57349
        , localTimeOfDay = TimeOfDay 23 5 0
        }
      , stampVersion         = "1.0"
      , uuid                 = "12121212-1212-1212-1212-121212121212"
      }
    }
  , concepts          =
    [ Concept
      { amount          = "112.00"
      , description     = "Product 1 Description"
      , _id             = Just "Product 1 ID"
      , importInfo      =
        [ ImportInfo
          { custom         = Just "Product 1 Custom"
          , importIssuedAt = ModifiedJulianDay 46664
          , importNumber   = "Product 1 Import Number"
          }
        ]
      , parts           = []
      , propertyAccount = Nothing
      , quantity        = "1.00"
      , unit            = "Product 1 Unit"
      , unitAmount      = "112.00"
      }
    , Concept
      { amount          = "424.24"
      , description     = "Product 2 Description"
      , _id             = Just "Product 2 ID"
      , importInfo      = []
      , parts           = []
      , propertyAccount = Nothing
      , quantity        = "2.00"
      , unit            = "Product 2 Unit"
      , unitAmount      = "212.12"
      }
    ]
  , currency          = Just "USD"
  , discount          = Just "24.36"
  , discountReason    = Just "Testing"
  , exchangeRate      = Just "17.1212"
  , internalID        = Just "144144"
  , issuedAt          = LocalTime
    { localDay       = ModifiedJulianDay 57916
    , localTimeOfDay = TimeOfDay 12 0 0
    }
  , issuedIn          = "Issued In"
  , issuer            = Issuer
    { fiscalAddress   = Just FiscalAddress
      { fiscalCountry        = "Fiscal Country"
      , fiscalExternalNumber = Just "Fiscal External Number"
      , fiscalInternalNumber = Just "Fiscal Internal Number"
      , fiscalLocality       = Just "Fiscal Locality"
      , fiscalMunicipality   = "Fiscal Municipality"
      , fiscalReference      = Just "Fiscal Reference"
      , fiscalState          = "Fiscal State"
      , fiscalStreet         = "Fiscal Street"
      , fiscalSuburb         = Just "Fiscal Suburb"
      , fiscalZipCode        = "22000"
      }
    , issuedInAddress = Just Address
      { country        = "Branch Country"
      , externalNumber = Just "Branch External Number"
      , internalNumber = Just "Branch Internal Number"
      , locality       = Just "Branch Locality"
      , municipality   = Just "Branch Municipality"
      , reference      = Just "Branch Reference"
      , state          = Just "Branch State"
      , street         = Just "Branch Street"
      , suburb         = Just "Branch Suburb"
      , zipCode        = Just "22114"
      }
    , name            = Just "Issuer Name"
    , regimes         = [TaxRegime "Fiscal Regime"]
    , rfc             = "XAXX010101000"
    }
  , originalAmount    = Just "1200.00"
  , originalIssuedAt  = Just LocalTime
    { localDay       = ModifiedJulianDay 57765
    , localTimeOfDay = TimeOfDay 12 12 12
    }
  , originalNumber    = Just "3636"
  , originalSeries    = Just "B"
  , paymentConditions = Just "Payment Conditions"
  , paymentMethod     = "99"
  , recipient         = Recipient
    { recipientAddress = Just Address
      { country        = "Recipient Country"
      , externalNumber = Just "Recipient External Number"
      , internalNumber = Just "Recipient Internal Number"
      , locality       = Just "Recipient Locality"
      , municipality   = Just "Recipient Municipality"
      , reference      = Just "Recipient Reference"
      , suburb         = Just "Recipient Suburb"
      , state          = Just "Recipient State"
      , street         = Just "Recipient Street"
      , zipCode        = Just "22116"
      }
    , recipientName    = Just "Recipient Name"
    , recipientRfc     = "XEXX010101000"
    }
  , series            = Just "A"
  , subTotal          = "536.192"
  , signature         = "Signature"
  , taxes             = Taxes
    { retainedTaxes   =
      [ RetainedTax
        { retainedTaxAmount = "12.144"
        , retainedTax       = ISR
        }
      ]
    , transferedTaxes =
      [ TransferedTax
        { transferedTaxAmount = "12.12"
        , transferedTaxRate   = "16"
        , transferedTax       = IVA
        }
      ]
    , totalRetained   = Just "12.144"
    , totalTransfered = Just "12.12"
    }
  , total             = "536.216"
  , _type             = "ingreso"
  , version           = "3.2"
  , wayToPay          = "Way To Pay"
  }

spec :: Spec
spec = do
  describe "CFDI.CSD.csdKeyToPem" $ do
    it "transforms CSD key from DER to PEM format" $ do
      eitherErrOrPem <-
        csdKeyToPem "test/csd/CSD01_AAA010101AAA.key" "12345678a"
      eitherErrOrPem `shouldSatisfy` isRight
      let Right pem = eitherErrOrPem
      testPEM <- readFile "test/csd/CSD01_AAA010101AAA.pem"
      pem `shouldBe` testPEM

  describe "CFDI.CSD.signWithCSD" $ do
    it "Signs some text with a CSD PEM" $ do
      eitherErrOrText <- signWithCSD "test/csd/CSD01_AAA010101AAA.pem" "TEST"
      eitherErrOrText `shouldSatisfy` isRight
      let Right text = eitherErrOrText
      text `shouldBe` testSignedText

  describe "CFDI.CSD.signCFDIWith" $ do
    it "Signs a CFDI with a CSD PEM" $ do
      eitherErrOrCFDI <- signCFDIWith "test/csd/CSD01_AAA010101AAA.pem" cfdi
      let Right signedCFDI = eitherErrOrCFDI
      signature signedCFDI `shouldBe` testCFDISignature

testCFDISignature :: Text
testCFDISignature =
  "SreR9muwTWUELA5YH78zICbtsmRBusbseyrQz0rNNy53KsE6lq3hwTSwOb3n3ySzx6hHYQ5VZGeC\
  \H20Fe+O8s2ThZayiwr457L7aElEy5SK8qYLuxqa9mRl3Y6IOc8CWNR19WNVfSYWjy3RP9ekTEI+j\
  \1Pg+qTqgMz/UaZbd1EOZstL6laeoCgPbZ5UxXoML+DtcZFcWABQqTbIoAh0fGC//l5h7X+umYh+B\
  \0/euVp+GzBX5gTU6ak9uBkmROT6laK6bHrDIGxeILHdHOMd3YX6VmnhgKJCvAKEtuogtJIN61AAM\
  \JPa/NpYmMYjtmWMeCNpYGQ5UE4ckOFNte7R3lA=="

testSignedText :: Text
testSignedText =
  "kopDKx5K63tyd/ZEQVWR9AJs9wi7qfK6veN3IYCRjeXHusfyMDKQizvUmQhsuuj3QkY/NvU3PpqP\
  \apJSNsbHQJxsTNTtsvxaCZxjV0HkuUp39ppHYeKM36Ed76UtN/0hMqOqYL29CIS8QOD+NCSjSDxm\
  \lk5CcJNgW9sQC/Kfe/w1E//tx9nWYenBVAI8gPfH3eA/wFm2glZAR0RHmWKQizq+HlugV7HeZnLk\
  \xnlMewgp8Ayr7D4D0skrbhwhPkkFwXoEHmux5/oUAHCX2LlTh0eLt9fCWjFJ2yOxY+S7bt9apIyA\
  \AO0nvzz1n7b042174OUICjtibQsoVkQLK8zjPw=="
