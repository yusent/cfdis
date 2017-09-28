{-# LANGUAGE OverloadedStrings #-}

module CFDI.V3_2.PAC.ITimbreSpec
  ( spec
  ) where

import CFDI.V3_2             (certificate, issuedAt, parse, signCFDIWith, stamp)
import CFDI.V3_2.PAC.ITimbre
import Data.Either           (isRight)
import Data.Text             (Text)
import Data.Time.LocalTime   (localDay)
import Data.Time.Calendar    (addDays)
import Data.Time.Clock       (getCurrentTime)
import Data.Time.Format      (defaultTimeLocale, formatTime, parseTimeM)
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

spec :: Spec
spec = do
  let credsFilePath = "test/yaml/pac-credentials/itimbre.yml"
  credsFileExist <- runIO $ doesFileExist credsFilePath

  if credsFileExist
    then do
      describe "CFDI.V3_2.PAC.ITimbre.ITimbre instance of PAC" $ do
        it "implements getPacStamp function" $ do
          Just (ITimbreCreds usr pass_ rfc_ crt pem) <-
            decodeFile credsFilePath

          xmlSource <- readFile "test/xml/invoice-for-stamp.xml"
          pemFilePath <- writeSystemTempFile "csd.pem" pem
          currentTimeStr <- formatTime defaultTimeLocale f <$> getCurrentTime
          now <- parseTimeM True defaultTimeLocale f currentTimeStr
          let Right baseCfdi = parse xmlSource
              cfdi = baseCfdi { certificate = crt, issuedAt = time }
              time = now { localDay = addDays (-1) (localDay now) }
          Right signedCfdi <- signCFDIWith pemFilePath cfdi
          eitherErrOrStamp <- stamp signedCfdi $ ITimbre usr pass_ rfc_ Testing
          eitherErrOrStamp `shouldSatisfy` isRight
          removeFile pemFilePath
    else
      return ()
  where
    f = "%Y-%m-%d-%H-%M-%S-%Z"
