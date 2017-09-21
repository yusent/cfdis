module CFDI.CSD where

import Data.ByteString           (empty)
import Data.ByteString.Base64    (encode)
import Data.Maybe                (fromJust)
import Data.Text                 (Text, concat, pack, split, unpack)
import Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import Data.Time.LocalTime       (LocalTime)
import Data.Time.Format          (defaultTimeLocale, parseTimeM)
import Prelude            hiding (concat)
import System.Exit               (ExitCode(..))
import System.Process.ByteString (readProcessWithExitCode)

data CsdCerData = CsdCerData
  { cerExpiresAt :: LocalTime
  , cerNumber    :: Text
  , cerToText    :: Text
  } deriving (Show)

-- TODO: Rewrite these methods using OpenSSL module. At the time of writting
-- this I couldn't figure out how to use it.

getCsdCerData :: FilePath -> IO (Either Text CsdCerData)
getCsdCerData cerPath = do
  pemOrErr <- getPem

  case pemOrErr of
    Right pem -> do
      serialOrErr <- getSerial $ encodeUtf8 pem

      case serialOrErr of
        Right serial -> do
          endDateOrErr <- getEndDate $ encodeUtf8 pem

          case endDateOrErr of
            Right endDate -> do
              return . Right . CsdCerData endDate serial $ sha1 pem

            Left err -> return $ Left err

        Left err -> return $ Left err

    Left err -> return $ Left err
  where
    getPem = do
      let args = ["x509", "-inform", "DER", "-outform", "PEM", "-in", cerPath]
      getResult <$> readProcessWithExitCode "openssl" args empty

    getSerial pem = do
      let args = ["x509", "-noout", "-serial"]
      parseSerial . getResult <$> readProcessWithExitCode "openssl" args pem
      where
        parseSerial = fmap (pack . odds . unpack . head . tail . split (== '='))
        odds [] = []
        odds [x] = []
        odds (e1 : e2 : xs) = e2 : odds xs

    getEndDate pem = do
      let args = ["x509", "-noout", "-enddate"]
      parseEndDate . getResult <$> readProcessWithExitCode "openssl" args pem
      where
        parseEndDate = fmap (fromJust . parseTimeM True defaultTimeLocale format . unpack)
        format = "notAfter=%b %d %H:%M:%S %Y %Z"

    getResult (exitCode, stdout, stderr) =
      case exitCode of
        ExitSuccess   -> Right $ decodeUtf8 stdout
        ExitFailure _ -> Left  $ decodeUtf8 stderr

    sha1 = concat . init . init . tail . split (== '\n')

csdKeyToPem :: FilePath -> String -> IO (Either Text Text)
csdKeyToPem keyPath keyPass = do
  let pass = "pass:" ++ keyPass
  let args = ["pkcs8", "-inform", "DER", "-in", keyPath, "-passin", pass]
  (exitCode, stdout, stderr) <- readProcessWithExitCode "openssl" args empty
  return $ case exitCode of
    ExitSuccess   -> Right $ decodeUtf8 stdout
    ExitFailure _ -> Left  $ decodeUtf8 stderr

signWithCSD :: FilePath -> Text -> IO (Either Text Text)
signWithCSD csdPemPath txt = do
  let args = ["dgst", "-sha1", "-sign", csdPemPath]
  (exitCode, stdout, stderr) <-
    readProcessWithExitCode "openssl" args $ encodeUtf8 txt
  return $ case exitCode of
    ExitSuccess   -> Right . decodeUtf8 $ encode stdout
    ExitFailure _ -> Left $ decodeUtf8 stderr
