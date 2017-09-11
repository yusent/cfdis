module CFDI.ParserSpec (spec) where

import CFDI
import CFDI.Parser
import Data.Time.Calendar  (Day(ModifiedJulianDay))
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Test.Hspec

spec :: Spec
spec = do
  describe "CFDI.Parser.parseCFDI" $ do
    invoiceCFDI <- parseCFDI <$> runIO (readFile "test/xml/invoice.xml")

    it "parses invoices" $ do
      invoiceCFDI `shouldBe` Just CFDI
        { accountNumber     = Just "1212"
        , certificate       = "SOMENOTSORANDOMCERTIFICATE"
        , certificateNumber = "00001000001212121212"
        , complement        = Just Complement
          { pacStamp = Just PacStamp
            { cfdSignature         = "Signature"
            , satCertificateNumber = "SAT Certificate Number"
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
            { amount          = 112
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
            , quantity        = 1
            , unit            = "Product 1 Unit"
            , unitAmount      = 112
            }
          , Concept
            { amount          = 424.24
            , description     = "Product 2 Description"
            , _id             = Just "Product 2 ID"
            , importInfo      = []
            , parts           = []
            , propertyAccount = Nothing
            , quantity        = 2
            , unit            = "Product 2 Unit"
            , unitAmount      = 212.12
            }
          ]
        , currency          = Just "MXN"
        , discount          = Just 24.36
        , discountReason    = Just "Testing"
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
            , fiscalZipCode        = "Fiscal Zip Code"
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
            , zipCode        = Just "Branch Zip Code"
            }
          , name            = Just "Issuer Name"
          , regimes         = [TaxRegime "Fiscal Regime"]
          , rfc             = "XAXX010101000"
          }
        , originalAmount    = Just 1200
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
            , zipCode        = Just "Recipient Zip Code"
            }
          , recipientName    = Just "Recipient Name"
          , recipientRfc     = "XEXX010101000"
          }
        , series            = Just "A"
        , subTotal          = 536.192
        , signature         = "Signature"
        , taxes             = Taxes
          { retainedTaxes   =
            [ RetainedTax
              { retainedTaxAmount = 12.144
              , retainedTax       = ISR
              }
            ]
          , transferedTaxes =
            [ TransferedTax
              { transferedTaxAmount = 12.12
              , transferedTaxRate   = 16
              , transferedTax       = IVA
              }
            ]
          , totalRetained   = Just 12.144
          , totalTransfered = Just 12.12
          }
        , total             = 536.216
        , _type             = "ingreso"
        , version           = "3.2"
        , wayToPay          = "Way To Pay"
        }
