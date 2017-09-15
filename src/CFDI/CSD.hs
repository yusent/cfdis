module CFDI.CSD where

import CFDI
import CFDI.Chain                (originalChain)
import Data.ByteString           (empty)
import Data.ByteString.Base64    (encode)
import Data.Text                 (Text)
import Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import System.Exit               (ExitCode(..))
import System.Process.ByteString (readProcessWithExitCode)

-- TODO: Rewrite these methods using OpenSSL module. At the time of writting
-- this I couldn't figure out how to use it.

csdKeyToPem :: FilePath -> String -> IO (Either Text Text)
csdKeyToPem keyPath keyPass = do
  let pass = "pass:" ++ keyPass
  let args = ["pkcs8", "-inform", "DER", "-in", keyPath, "-passin", pass]
  (exitCode, stdout, stderr) <- readProcessWithExitCode "openssl" args empty
  return $ case exitCode of
    ExitSuccess   -> Right $ decodeUtf8 stdout
    ExitFailure _ -> Left  $ decodeUtf8 stderr

signCFDIWith :: FilePath -> CFDI -> IO (Either Text CFDI)
signCFDIWith csdPemPath cfdi = do
  eitherErrOrSignature <- signWithCSD csdPemPath $ originalChain cfdi
  return $ case eitherErrOrSignature of
    Right sig -> Right $ cfdi { signature = sig }
    Left  err -> Left err

signWithCSD :: FilePath -> Text -> IO (Either Text Text)
signWithCSD csdPemPath txt = do
  let args = ["dgst", "-sha1", "-sign", csdPemPath]
  (exitCode, stdout, stderr) <- readProcessWithExitCode "openssl" args $ encodeUtf8 txt
  return $ case exitCode of
    ExitSuccess   -> Right . decodeUtf8 $ encode stdout
    ExitFailure _ -> Left $ decodeUtf8 stderr
