{-# LANGUAGE OverloadedStrings #-}

module CFDI.PAC.ITimbreSpec
  ( spec
  ) where

import CFDI
import CFDI.PAC            (stamp)
import CFDI.PAC.ITimbre
import Data.Either         (isRight)
import Data.Text           (Text, unpack)
import Data.Time.Calendar  (Day(ModifiedJulianDay))
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..), localDay)
import Data.Time.Calendar  (addDays)
import Data.Time.Clock     (getCurrentTime)
import Data.Time.Format    (defaultTimeLocale, formatTime, parseTimeM)
import Data.Yaml
  ( FromJSON
  , Value(Object)
  , (.:)
  , decodeFile
  , parseJSON
  )
import System.Directory    (doesFileExist, removeFile)
import System.IO.Temp      (writeSystemTempFile)
import Test.Hspec

data ITimbreCreds = ITimbreCreds Text Text Text Text String

instance FromJSON ITimbreCreds where
  parseJSON (Object v) = ITimbreCreds
    <$> v .: "user"
    <*> v .: "pass"
    <*> v .: "rfc"
    <*> v .: "csdCert"
    <*> v .: "csdPem"

cfdi :: CFDI
cfdi = CFDI
  (Just (CertificateNumber "00001000000403544254"))
  Nothing
  Income
  []
  (Concepts
    [ Concept
        (Amount 1090.52)
        []
        (ProductDescription "COMIDA MEXICANA")
        Nothing
        MU_ACT
        (Just (ProductId "PROD12"))
        (ProductOrService 91111700)
        (Quantity 1)
        (Just (ConceptTaxes
                Nothing
                (Just (ConceptTransferedTaxes
                        [ ConceptTransferedTax
                            (Amount 174.48)
                            (TaxBase 1090.52)
                            Rate
                            (TaxRate 0.16)
                            IVA
                        ]))))
        (Just (ProductUnit "NA"))
        (Amount 1090.52)
    ])
  Nothing
  CUR_MXN
  Nothing
  Nothing
  (Just (Folio "12"))
  (LocalTime
    (ModifiedJulianDay 57953)
    (TimeOfDay 14 27 3))
  (ZipCode 22115)
  (Issuer
    (Just (Name "EMISOR DE PRUEBA"))
    (RFC "LEVM590117199")
    (PeopleWithBusinessActivities))
  (Just (PaymentConditions "CONDICIONES DE PAGO DE PRUEBA"))
  (Just OneTimePayment)
  (Recipient
    (Just GeneralExpenses)
    (Just (Name "RECEPTOR DE PRUEBA"))
    (RFC "XAXX010101000")
    Nothing
    Nothing)
  Nothing
  (Just (Series "ABC"))
  Nothing
  (Amount 1090.52)
  (Just (Taxes 
          Nothing
          (Just (Amount 174.48))
          Nothing
          (Just (TransferedTaxes
                  [ TransferedTax 
                      (Amount 174.48)
                      Rate
                      (TaxRate 0.16)
                      IVA
                  ]))))
  (Amount 1265)
  (Version 3.3)
  (Just Cash)

spec :: Spec
spec = do
  let credsFilePath = "test/yaml/pac-credentials/itimbre.yml"
  credsFileExist <- runIO $ doesFileExist credsFilePath

  if credsFileExist
    then do
      describe "CFDI.PAC.ITimbre.ITimbre instance of PAC" $ do
        it "implements getPacStamp function" $ do
          Just (ITimbreCreds usr pass_ rfc_ crt pem) <-
            decodeFile credsFilePath

          pemFilePath <- writeSystemTempFile "csd.pem" pem
          currentTimeStr <- formatTime defaultTimeLocale f <$> getCurrentTime
          now <- parseTimeM True defaultTimeLocale f currentTimeStr
          let cfdi' = cfdi
                { certText = Just crt
                , issuedAt = time
                }
              time = now { localDay = addDays (-1) (localDay now) }
          Right signedCfdi <- signWith pemFilePath cfdi'
          eitherErrOrStamp <- stamp signedCfdi $ ITimbre usr pass_ rfc_ Testing
          eitherErrOrStamp `shouldSatisfy` isRight
          removeFile pemFilePath
    else
      return ()
  where
    f = "%Y-%m-%d-%H-%M-%S-%Z"
