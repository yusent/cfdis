{-# LANGUAGE OverloadedStrings #-}

import CFDI (parseCfdiXml, ppXmlParseError, signWith, toXML)
import CFDI.PAC (ppStampError, stampWithRetry)
import CFDI.PAC.Dummy (Dummy(..))
import CFDI.PAC.Fel (Fel(Fel), FelEnv(..))
import CFDI.PAC.ITimbre (ITimbre(ITimbre), ITimbreEnv(..))
import qualified Data.ByteString as BS (getContents)
import qualified Data.ByteString.Char8 as C8 (putStrLn)
import Data.Char (toLower)
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Environment (getArgs, getEnv, lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs

  case args of
    (csdPemPath : pacName : _) -> do
      eitherErrOrParsedCfdi <- parseCfdiXml . decodeUtf8 <$> BS.getContents

      case eitherErrOrParsedCfdi of
        Left parseErr -> do
          hPutStrLn stderr $ ppXmlParseError "  " parseErr
          exitWith $ ExitFailure 3

        Right parsedCfdi -> do
          eitherErrorOrSignedCfdi <- signWith csdPemPath parsedCfdi

          case eitherErrorOrSignedCfdi of
            Left signErr -> do
              hPutStrLn stderr $ unpack signErr
              exitWith $ ExitFailure 4

            Right signedCfdi -> do
              environment <- lookupEnv "CFDI_ENVIRONMENT"
              let isTest = (map toLower <$> environment) == Just "test"

              case map toLower pacName of
                "itimbre" -> do
                  user <- getEnv "CFDI_ITIMBRE_USER"
                  pass <- getEnv "CFDI_ITIMBRE_PASS"
                  rfc <- getEnv "CFDI_ITIMBRE_RFC"

                  let pac = ITimbre
                        (pack user)
                        (pack pass)
                        (pack rfc)
                        ""
                        ""
                        (if isTest then Testing else Production)

                  eitherErrOrStampedCfdi <- stampWithRetry signedCfdi pac

                  case eitherErrOrStampedCfdi of
                    Left stampErr -> do
                      hPutStrLn stderr $ ppStampError stampErr
                      exitWith $ ExitFailure 6

                    Right stampedCfdi -> do
                      putStrLn $ toXML stampedCfdi

                "fel" -> do
                  user <- getEnv "CFDI_FEL_USER"
                  pass <- getEnv "CFDI_FEL_PASS"
                  rfc <- getEnv "CFDI_FEL_RFC"

                  let pac = Fel
                        (pack user)
                        (pack pass)
                        (pack rfc)
                        ""
                        ""
                        (if isTest then FelTestingEnv else FelProductionEnv)

                  eitherErrOrStampedCfdi <- stampWithRetry signedCfdi pac

                  case eitherErrOrStampedCfdi of
                    Left stampErr -> do
                      hPutStrLn stderr $ ppStampError stampErr
                      exitWith $ ExitFailure 6

                    Right stampedCfdi -> do
                      C8.putStrLn . encodeUtf8 . pack $ toXML stampedCfdi

                "dummy" -> do
                  eitherErrOrStampedCfdi <- stampWithRetry signedCfdi Dummy

                  case eitherErrOrStampedCfdi of
                    Left stampErr -> do
                      hPutStrLn stderr $ ppStampError stampErr
                      exitWith $ ExitFailure 6

                    Right stampedCfdi -> do
                      C8.putStrLn . encodeUtf8 . pack $ toXML stampedCfdi

                unknownPac -> do
                  hPutStrLn stderr $  "Unknown PAC " ++ unknownPac
                  exitWith $ ExitFailure 5

    _ -> do
      hPutStrLn stderr $ "Usage: stamp [Path to CSD PEM file] [PAC name]"
      exitWith $ ExitFailure 2
