{-# LANGUAGE OverloadedStrings #-}

module CFDISpec (spec) where

import CFDI
import CFDI.Parser
import CFDI.Types
import Data.Either         (isRight)
import Data.List.Extra     (replace)
import Data.Text           (Text)
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

originalChain' :: Text
originalChain' =
  "||3.2|2017-06-12T12:00:00|ingreso|Way To Pay|Payment Conditions|536.192\
  \|24.36|17.1212|USD|536.216|99|Issued In|1212|3636|B|2017-01-12T12:12:12\
  \|1200.00|XAXX010101000|Issuer Name|Fiscal Street|Fiscal External Number\
  \|Fiscal Internal Number|Fiscal Suburb|Fiscal Locality|Fiscal Reference\
  \|Fiscal Municipality|Fiscal State|Fiscal Country|22000|Branch Street\
  \|Branch External Number|Branch Internal Number|Branch Suburb|Branch Locality\
  \|Branch Reference|Branch Municipality|Branch State|Branch Country|22114\
  \|Fiscal Regime|XEXX010101000|Recipient Name|Recipient Street\
  \|Recipient External Number|Recipient Internal Number|Recipient Suburb\
  \|Recipient Locality|Recipient Reference|Recipient Municipality\
  \|Recipient State|Recipient Country|22116|1.00|Product 1 Unit|Product 1 ID\
  \|Product 1 Description|112.00|112.00|Product 1 Import Number|1986-08-22\
  \|Product 1 Custom|2.00|Product 2 Unit|Product 2 ID|Product 2 Description\
  \|212.12|424.24|ISR|12.144|12.144|IVA|16|12.12|12.12||"

spec :: Spec
spec = do
  describe "CFDI.originalChain" $ do
    it "calculates an original chain" $ do
      originalChain cfdi `shouldBe` originalChain'

  describe "CFDI.parse" $ do
    xmlSource <- runIO $ readFile "test/xml/invoice.xml"

    it "parses invoices" $ do
      parse xmlSource `shouldBe` Right cfdi

    it "throws parse errors" $ do
      let badSource0 = replace "calle=" "blah=" xmlSource

      parse badSource0 `shouldBe` Left
        ( ParseErrorInChild "Emisor"
          ( ParseErrorInChild "DomicilioFiscal"
            ( AttrNotFound "calle"
            )
          )
        )

      let badSource1 = replace "cantidad=" "blah=" xmlSource

      parse badSource1 `shouldBe` Left
        ( ParseErrorInChild "Conceptos"
          ( ParseErrorInChild "Concepto"
            ( AttrNotFound "cantidad"
            )
          )
        )

      let badSource2 = replace "1986-08-22" "blah" xmlSource

      parse badSource2 `shouldBe` Left
        ( ParseErrorInChild "Conceptos"
          ( ParseErrorInChild "Concepto"
            ( ParseErrorInChild "InformacionAduanera"
              ( InvalidFormat "fecha"
              )
            )
          )
        )

      let badSource3 = replace "2017-06-12T12:00:00" "blah" xmlSource

      parse badSource3 `shouldBe` Left (InvalidFormat "fecha")

  describe "CFDI.signCFDIWith" $ do
    it "signs a CFDI with a CSD PEM" $ do
      eitherErrOrCFDI <- signCFDIWith "test/csd/CSD01_AAA010101AAA.pem" cfdi
      eitherErrOrCFDI `shouldSatisfy` isRight
      let Right signedCFDI = eitherErrOrCFDI
      signature signedCFDI `shouldBe` testCFDISignature

  describe "CFDI.toXML" $ do
    it "renders a complete representation of a CFDI as XML" $ do
      parse (toXML cfdi) `shouldBe` Right cfdi

testCFDISignature :: Text
testCFDISignature =
  "SreR9muwTWUELA5YH78zICbtsmRBusbseyrQz0rNNy53KsE6lq3hwTSwOb3n3ySzx6hHYQ5VZGeC\
  \H20Fe+O8s2ThZayiwr457L7aElEy5SK8qYLuxqa9mRl3Y6IOc8CWNR19WNVfSYWjy3RP9ekTEI+j\
  \1Pg+qTqgMz/UaZbd1EOZstL6laeoCgPbZ5UxXoML+DtcZFcWABQqTbIoAh0fGC//l5h7X+umYh+B\
  \0/euVp+GzBX5gTU6ak9uBkmROT6laK6bHrDIGxeILHdHOMd3YX6VmnhgKJCvAKEtuogtJIN61AAM\
  \JPa/NpYmMYjtmWMeCNpYGQ5UE4ckOFNte7R3lA=="
