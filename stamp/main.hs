{-# LANGUAGE OverloadedStrings #-}

import CFDI (parseCfdiFile, ppXmlParseError, signWith, toXML)
import CFDI.PAC (ppStampError, stampWithRetry)
import CFDI.PAC.ITimbre (ITimbre(ITimbre), ITimbreEnv(Production))
import Data.Char (toLower)
import Data.Text (pack, unpack)
import System.Environment (getArgs, getEnv)
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
          pacName <- map toLower <$> getEnv "STAMP_PAC"

          case pacName of
            "itimbre" -> do
              user <- getEnv "STAMP_ITIMBRE_USER"
              pass <- getEnv "STAMP_ITIMBRE_PASS"
              rfc <- getEnv "STAMP_ITIMBRE_RFC"

              let pac =
                    ITimbre (pack user) (pack pass) (pack rfc) "" "" Production

              eitherErrOrStampedCfdi <- stampWithRetry signedCfdi pac

              case eitherErrOrStampedCfdi of
                Left stampErr -> do
                  hPutStrLn stderr $ ppStampError stampErr
                  exitFailure

                Right stampedCfdi -> do
                  putStrLn $ toXML stampedCfdi

            unknownPac -> do
              hPutStrLn stderr $ "Unknown PAC " ++ unknownPac
              exitFailure
