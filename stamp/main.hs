{-# LANGUAGE OverloadedStrings #-}

import CFDI (parseCfdiFile, ppXmlParseError, signWith, toXML)
import CFDI.PAC (ppStampError, stampWithRetry)
import CFDI.PAC.Fel (Fel(Fel), FelEnv(FelProductionEnv))
import CFDI.PAC.ITimbre (ITimbre(ITimbre), ITimbreEnv(Production))
import Data.Bifunctor (first)
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

          eitherErrOrStampedCfdi <- case pacName of
            "itimbre" -> do
              user <- getEnv "STAMP_ITIMBRE_USER"
              pass <- getEnv "STAMP_ITIMBRE_PASS"
              rfc <- getEnv "STAMP_ITIMBRE_RFC"

              let pac =
                    ITimbre (pack user) (pack pass) (pack rfc) "" "" Production

              first ppStampError <$> stampWithRetry signedCfdi pac

            "fel" -> do
              user <- getEnv "STAMP_FEL_USER"
              pass <- getEnv "STAMP_FEL_PASS"
              rfc <- getEnv "STAMP_FEL_RFC"

              let pac =
                    Fel (pack user) (pack pass) (pack rfc) "" "" FelProductionEnv

              first ppStampError <$> stampWithRetry signedCfdi pac

            unknownPac -> do
              return . Left $ "Unknown PAC " ++ unknownPac

          case eitherErrOrStampedCfdi of
            Left stampErr -> do
              hPutStrLn stderr stampErr
              exitFailure

            Right stampedCfdi -> do
              putStrLn $ toXML stampedCfdi
