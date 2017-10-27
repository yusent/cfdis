module CFDI.CSD
  ( CsdCerData(..)
  , csdKeyToPem
  , exportCsdAsPfx
  , getCsdCerData
  , signWithCSD
  ) where

import Control.Exception         (ErrorCall, catch, evaluate)
import Data.Bifunctor            (first)
import Data.ByteString           (ByteString)
import Data.ByteString.Base64    (encode)
import Data.Text                 (Text, concat, empty, pack, split, unpack)
import Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import Data.Time.LocalTime       (LocalTime)
import Data.Time.Format          (defaultTimeLocale, parseTimeM)
import Prelude            hiding (concat)
import System.Exit               (ExitCode(..))
import System.Process.ByteString (readProcessWithExitCode)
import qualified Util as U       (split)

data CsdCerData = CsdCerData
  { cerExpiresAt :: LocalTime
  , cerNumber    :: Text
  , cerToText    :: Text
  } deriving (Eq, Show)

-- TODO: Rewrite these methods using OpenSSL module. At the time of writting
-- this I couldn't figure out how to use it.

csdKeyToPem :: FilePath -> String -> IO (Either Text Text)
csdKeyToPem keyPath keyPass =
  runOpenSSL cmd empty
  where
    cmd = "pkcs8 -inform DER -in " ++ keyPath ++ " -passin pass:" ++ keyPass

exportCsdAsPfx :: FilePath -> Text -> Text -> IO (Either Text Text)
exportCsdAsPfx keyPath cerPem pwd =
  runOpenSSLB64 cmd cerPem
  where
    cmd = "pkcs12 -export -inkey " ++ keyPath ++ " -passout pass:" ++ unpack pwd

getCsdCerData :: FilePath -> IO (Either Text CsdCerData)
getCsdCerData cerPath =
  getPem cerPath >>= eitherErrOrContinue (\pem ->
    getSerial pem >>= eitherErrOrContinue (\serial ->
      getEndDate pem >>= eitherErrOrContinue (\endDate ->
        return . Right . CsdCerData endDate serial $ sha1 pem)))
  where
    eitherErrOrContinue = either (return . Left)
    sha1 = concat . init . init . tail . split (== '\n')

signWithCSD :: FilePath -> Text -> IO (Either Text Text)
signWithCSD csdPemPath = runOpenSSLB64 ("dgst -sha1 -sign " ++ csdPemPath)

-- Helpers

getPem :: FilePath -> IO (Either Text Text)
getPem cerPath =
  runOpenSSL ("x509 -inform DER -outform PEM -in " ++ cerPath) empty

getSerial :: Text -> IO (Either Text Text)
getSerial pem =
  parseSerial <$> runOpenSSL "x509 -noout -serial" pem
  where
    parseSerial = fmap (pack . odds . unpack . head . tail . split (== '='))
    odds [] = []
    odds [_] = []
    odds (_ : e2 : xs) = e2 : odds xs

getEndDate :: Text -> IO (Either Text LocalTime)
getEndDate pem = catch
  (runOpenSSL "x509 -noout -enddate" pem >>= evaluate . parseEndDate)
  handleErr
  where
    handleErr :: ErrorCall -> IO (Either Text LocalTime)
    handleErr _ = return $ Left "Formato de fecha de expiración inválido"
    parseEndDate = (>>= parseTimeM True defaultTimeLocale format . unpack)
    format = "notAfter=%b %d %H:%M:%S %Y %Z"

runOpenSSL :: String -> Text -> IO (Either Text Text)
runOpenSSL command =
  fmap (first decodeUtf8 . fmap decodeUtf8) . runOpenSSL_ command

runOpenSSLB64 :: String -> Text -> IO (Either Text Text)
runOpenSSLB64 command =
  fmap (first decodeUtf8 . fmap (decodeUtf8 . encode)) . runOpenSSL_ command

runOpenSSL_ :: String -> Text -> IO (Either ByteString ByteString)
runOpenSSL_ command stdin = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode "openssl" args bsStdin
  return $ case exitCode of
    ExitSuccess   -> Right stdout
    ExitFailure _ -> Left  stderr
  where
    args = U.split ' ' command
    bsStdin = encodeUtf8 stdin
