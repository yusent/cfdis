module CFDI.CSD
  ( CsdCerData(..)
  , getCsdCerData
  , csdKeyToPem
  , signWithCSD
  ) where

import Data.ByteString           (ByteString)
import Data.ByteString.Base64    (encode)
import Data.Maybe                (fromJust)
import Data.Text                 (Text, concat, empty, pack, split, unpack)
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
getCsdCerData cerPath =
  getPem cerPath >>= eitherErrOrContinue (\pem ->
    getSerial pem >>= eitherErrOrContinue (\serial ->
      getEndDate pem >>= eitherErrOrContinue (\endDate ->
        return . Right . CsdCerData endDate serial $ sha1 pem)))
  where
    eitherErrOrContinue = either (return . Left)
    sha1 = concat . init . init . tail . split (== '\n')

csdKeyToPem :: FilePath -> String -> IO (Either Text Text)
csdKeyToPem keyPath keyPass =
  runOpenSSL ["pkcs8", "-inform", "DER", "-in", keyPath, "-passin", pass] empty
  where
    pass = "pass:" ++ keyPass

signWithCSD :: FilePath -> Text -> IO (Either Text Text)
signWithCSD csdPemPath =
  fmap (either (Left . decodeUtf8) (Right . decodeUtf8 . encode))
    . runOpenSSL_ ["dgst", "-sha1", "-sign", csdPemPath]

-- Helpers

getPem :: FilePath -> IO (Either Text Text)
getPem cerPath =
  runOpenSSL ["x509", "-inform", "DER", "-outform", "PEM", "-in", cerPath] empty

getSerial :: Text -> IO (Either Text Text)
getSerial pem =
  parseSerial <$> runOpenSSL ["x509", "-noout", "-serial"] pem
  where
    parseSerial = fmap (pack . odds . unpack . head . tail . split (== '='))
    odds [] = []
    odds [x] = []
    odds (e1 : e2 : xs) = e2 : odds xs

getEndDate :: Text -> IO (Either Text LocalTime)
getEndDate pem =
  fmap parseEndDate <$> runOpenSSL ["x509", "-noout", "-enddate"] pem
  where
    parseEndDate = fromJust . parseTimeM True defaultTimeLocale format . unpack
    format = "notAfter=%b %d %H:%M:%S %Y %Z"

runOpenSSL :: [String] -> Text -> IO (Either Text Text)
runOpenSSL args =
  fmap (either (Left . decodeUtf8) (Right . decodeUtf8)) . runOpenSSL_ args

runOpenSSL_ :: [String] -> Text -> IO (Either ByteString ByteString)
runOpenSSL_ args stdin = do
  let bsStdin = encodeUtf8 stdin
  (exitCode, stdout, stderr) <- readProcessWithExitCode "openssl" args bsStdin
  return $ case exitCode of
    ExitSuccess   -> Right stdout
    ExitFailure _ -> Left  stderr
