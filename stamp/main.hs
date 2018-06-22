import CFDI (parseCfdiFile, ppXmlParseError, signWith, toXML)
import Data.Text (unpack)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  (cfdiPath : csdPemPath : _) <- getArgs
  eitherErrOrParsedCfdi <- parseCfdiFile cfdiPath

  case eitherErrOrParsedCfdi of
    Left parseErr -> do
      hPutStrLn stderr $ ppXmlParseError "  " parseErr
      exitFailure

    Right parsedCfdi -> do
      eitherErrorOrSignedCfdi <- signWith csdPemPath parsedCfdi

      case eitherErrorOrSignedCfdi of
        Left signErr -> do
          hPutStrLn stderr $ unpack signErr
          exitFailure

        Right signedCfdi -> do
          putStrLn $ toXML signedCfdi
