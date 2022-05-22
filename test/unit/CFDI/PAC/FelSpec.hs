{-# LANGUAGE OverloadedStrings #-}

module CFDI.PAC.FelSpec
  ( spec
  ) where

import CFDI
import CFDI.PAC
  ( StampError(PacError)
  , cancelCFDI
  , getPacStamp
  , stamp
  , stampLookup
  )
import CFDI.PAC.Fel
import Control.Monad       (when)
import Data.Either         (isLeft, isRight)
import Data.Maybe          (isNothing)
import Data.Text           (Text, take, unpack)
import Data.Time.Calendar  (Day(ModifiedJulianDay), addDays)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..), localDay)
import Data.Time.Clock     (getCurrentTime)
import Data.Time.Format    (defaultTimeLocale, formatTime, parseTimeM)
import Data.Yaml
  ( FromJSON
  , Value(Object)
  , (.:)
  , decodeFile
  , parseJSON
  )
import Prelude      hiding (take)
import System.Directory    (doesFileExist, removeFile)
import System.IO.Temp      (writeSystemTempFile)
import Test.Hspec

data FelCreds = FelCreds Text Text Text Text String Text Text Text

instance FromJSON FelCreds where
  parseJSON (Object v) = FelCreds
    <$> v .: "user"
    <*> v .: "pass"
    <*> v .: "rfc"
    <*> v .: "csdCert"
    <*> v .: "csdPem"
    <*> v .: "csdPfxPass"
    <*> v .: "csdPfxPem"
    <*> v .: "csdSerial"

cfdi :: CFDI
cfdi = CFDI
  Nothing
  Nothing
  Nothing
  Income
  Nothing
  (Concepts
    [ Concept
        (Amount 1090.52)
        []
        (ProductDescription "COMIDA MEXICANA O ESPAÑOLA")
        Nothing
        MU_ACT
        (Just (ProductId "PROD12"))
        (ProductOrService 91111700)
        Nothing
        (Quantity 1)
        (Just (ConceptTaxes
                Nothing
                (Just (ConceptTransferedTaxes
                        [ ConceptTransferedTax
                            (Just (Amount 174.48))
                            (Amount 1090.52)
                            Rate
                            (Just (TaxRate 0.16))
                            IVA
                        ]))))
        Nothing
        (Just (ProductUnit "NA"))
        (Amount 1090.52)
    ])
  Nothing
  CUR_MXN
  Nothing
  Nothing
  Nothing
  (Just (Folio "12"))
  (LocalTime
    (ModifiedJulianDay 57953)
    (TimeOfDay 14 27 3))
  (ZipCode 22115)
  (Issuer
    (Just (Name "EMISOR DE PRUEBA"))
    (RFC "TEST010203001")
    PeopleWithBusinessActivities)
  (Just (PaymentConditions "CONDICIONES DE PAGO DE PRUEBA"))
  (Just OneTimePayment)
  (Recipient
    (Just GeneralExpenses)
    (Just (Name "RECEPTOR DE PRUEBA"))
    (RFC "TES030201001")
    Nothing
    Nothing
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
  let credsFilePath = "test/yaml/pac-credentials/fel.yml"
  credsFileExist <- runIO $ doesFileExist credsFilePath

  when credsFileExist $ do
    (fel, pem, crt, crtNum) <- runIO $ do
      Just (FelCreds usr pass_ rfc_ crt pem pfxPwd pfxPem crtNum) <-
        decodeFile credsFilePath
      return (Fel usr pass_ rfc_ pfxPwd pfxPem FelTestingEnv, pem, crt, crtNum)

    describe "CFDI.PAC.Fel.Fel instance of PAC" $ do
      it "implements getPacStamp function" $ do
        currentTimeStr <- formatTime defaultTimeLocale f <$> getCurrentTime
        now <- parseTimeM True defaultTimeLocale f currentTimeStr
        pemFilePath <- writeSystemTempFile "csd.pem" pem
        let cfdi' = cfdi
              { certNum  = Just (CertificateNumber crtNum)
              , certText = Just crt
              , issuedAt = time
              }
            time = now { localDay = addDays (-1) (localDay now) }
        Right signedCfdi@CFDI{signature = Just sig} <-
          signWith pemFilePath cfdi'
        let cfdiId = take 12 sig
        eitherErrOrStamp <- getPacStamp signedCfdi fel cfdiId
        eitherErrOrStamp `shouldSatisfy` isRight
        removeFile pemFilePath

      it "implements stampLookup function" $ do
        -- We need to stamp a CFDI first to test this.
        currentTimeStr <- formatTime defaultTimeLocale f <$> getCurrentTime
        now <- parseTimeM True defaultTimeLocale f currentTimeStr
        pemFilePath <- writeSystemTempFile "csd.pem" pem
        let cfdi' = cfdi
              { certNum  = Just (CertificateNumber crtNum)
              , certText = Just crt
              , issuedAt = time
              }
            time = now { localDay = addDays (-1) (localDay now) }
        Right signedCfdi@CFDI{signature = Just sig} <-
          signWith pemFilePath cfdi'
        let cfdiId = take 12 sig
        eitherErrOrStamp <- getPacStamp signedCfdi fel cfdiId
        eitherErrOrStamp `shouldSatisfy` isRight

        eitherErrOrStamp' <- getPacStamp signedCfdi fel cfdiId
        eitherErrOrStamp' `shouldSatisfy` isLeft
        let Left (PacError _ code) = eitherErrOrStamp'
        code `shouldBe` Just "801"

        eitherErrOrStamp'' <- stampLookup fel cfdiId
        eitherErrOrStamp'' `shouldSatisfy` isRight
        removeFile pemFilePath
  where
    f = "%Y-%m-%d-%H-%M-%S-%Z"
