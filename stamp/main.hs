{-# LANGUAGE OverloadedStrings #-}

import CFDI (parseCfdiFile, ppXmlParseError, signWith, toXML)
import CFDI.PAC (ppStampError, stampWithRetry)
import CFDI.PAC.Fel (Fel(Fel), FelEnv(FelProductionEnv))
import CFDI.PAC.ITimbre (ITimbre(ITimbre), ITimbreEnv(Production))
import Data.Char (toLower)
import Data.Text (pack, unpack)
import System.Environment (getArgs, getEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  (cfdiPath : csdPemPath : _) <- getArgs
  eitherErrOrParsedCfdi <- parseCfdiFile cfdiPath

  case eitherErrOrParsedCfdi of
    Left parseErr -> do
      hPutStrLn stderr $ ppXmlParseError "  " parseErr
      exitWith $ ExitFailure 2

    Right parsedCfdi -> do
      eitherErrorOrSignedCfdi <- signWith csdPemPath parsedCfdi

      case eitherErrorOrSignedCfdi of
        Left signErr -> do
          hPutStrLn stderr $ unpack signErr
          exitWith $ ExitFailure 3

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
                  exitWith $ ExitFailure 5

                Right stampedCfdi -> do
                  putStrLn $ toXML stampedCfdi

            "fel" -> do
              user <- getEnv "STAMP_FEL_USER"
              pass <- getEnv "STAMP_FEL_PASS"
              rfc <- getEnv "STAMP_FEL_RFC"

              let pac =
                    Fel (pack user) (pack pass) (pack rfc) "" "" FelProductionEnv

              eitherErrOrStampedCfdi <- stampWithRetry signedCfdi pac

              case eitherErrOrStampedCfdi of
                Left stampErr -> do
                  hPutStrLn stderr $ ppStampError stampErr
                  exitWith $ ExitFailure 5

                Right stampedCfdi -> do
                  putStrLn $ toXML stampedCfdi

            unknownPac -> do
              hPutStrLn stderr $  "Unknown PAC " ++ unknownPac
              exitWith $ ExitFailure 4
