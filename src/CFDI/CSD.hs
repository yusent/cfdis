module CFDI.CSD where

import CFDI
import CFDI.Chain                (originalChain)
import Codec.Binary.UTF8.String  (decode)
import Data.ByteString           (ByteString, empty, unpack)
import Data.ByteString.Base64    (encode)
import System.Exit               (ExitCode(..))
import System.Process.ByteString (readProcessWithExitCode)

-- TODO: Rewrite these methods using module OpenSSL. At the time of writting
-- this I couldn't figure out how to use it.

csdKeyToPem :: FilePath -> String -> IO (Either ByteString ByteString)
csdKeyToPem keyPath keyPass = do
  let pass = "pass:" ++ keyPass
  let args = ["pkcs8", "-inform", "DER", "-in", keyPath, "-passin", pass]
  (exitCode, stdout, stderr) <- readProcessWithExitCode "openssl" args empty
  case exitCode of
    ExitSuccess   -> return $ Right stdout
    ExitFailure _ -> return $ Left stderr

signCFDIWith :: FilePath -> CFDI -> IO (Either String CFDI)
signCFDIWith csdPemPath cfdi = do
  eitherErrOrSignature <- signWithCSD csdPemPath $ originalChain cfdi
  return $ case eitherErrOrSignature of
    Right sig -> Right $ cfdi { signature = decode $ unpack sig }
    Left  err -> Left . decode $ unpack err

signWithCSD :: FilePath -> String -> IO (Either ByteString ByteString)
signWithCSD csdPemPath str = do
  let args = ["dgst", "-sha1", "-sign", csdPemPath]
  (exitCode, stdout, stderr) <- readProcessWithExitCode "openssl" args empty
  case exitCode of
    ExitSuccess   -> return . Right $ encode stdout
    ExitFailure _ -> return $ Left stderr
