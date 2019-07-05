{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CFDI.PAC.Fel where

import CFDI
  ( PacStamp
  , UUID(..)
  , getStampComplement
  , parseCfdiXml
  , toXML
  )
import CFDI.PAC
import CFDI.XmlNode
import Control.Error.Safe          (justErr)
import Control.Exception           (catch)
import Data.ByteString.Lazy        (ByteString, toStrict)
import Data.Maybe                  (listToMaybe)
import Data.Text                   (Text, pack, unpack)
import Data.Text.Encoding          (decodeUtf8)
import Network.HTTP.Client.TLS
import Network.SOAP
import Network.SOAP.Transport.HTTP
import Text.XML                    (Name(..))
import Text.XML.Writer hiding      (toXML)
import Text.XML.Light              (Element(..), cdData, onlyText, parseXMLDoc)

data Fel = Fel
  { felUser   :: Text
  , felPass   :: Text
  , felRfc    :: Text
  , felPfxPwd :: Text
  , felPfxPem :: Text
  , felEnv    :: FelEnv
  }

data FelEnv
  = FelProductionEnv
  | FelTestingEnv
  deriving (Eq, Show)

instance PAC Fel where
  cancelCFDI fel@Fel{..} (UUID uuid) = do
    f <- genFelWsFunc fel "CancelarCFDI" $ do
      element' "rFCEmisor" felRfc
      element' "listaCFDI" $ do
        element
          (Name
            "string"
            (Just "http://schemas.microsoft.com/2003/10/Serialization/Arrays")
            (Just "arr")
          )
          uuid
      element' "clavePrivada_Base64" felPfxPem
      element' "passwordClavePrivada" felPfxPwd
    f felCancelResponseParser `catch` handleCancelHttpException

  getPacStamp cfdi fel cfdiId = do
    f <- genFelWsFunc fel "TimbrarCFDI" $ do
      element' "cadenaXML" . pack $ toXML cfdi
      element' "referencia" cfdiId
    f (felStampResponseParser "TimbrarCFDI") `catch` handleStampHttpException

  stampLookup fel cfdiId = do
    f <- genFelWsFunc fel "ConsultarTimbrePorReferencia" $ do
      element' "referencia" cfdiId
    f (felStampResponseParser "ConsultarTimbrePorReferencia")
      `catch` handleStampHttpException

element' :: ToXML a => Text -> a -> XML
element' n = element (Name n (Just "http://tempuri.org/") (Just "tem"))

felCancelResponseParser :: ByteString -> Either CancelError Text
felCancelResponseParser xml = do
  resElem <-
    justErr (ParseCancelResponseError "Respuesta en formato desconocido.")
      $ mResElem

  let mDetailElem = findChildByName "DetallesCancelacion" resElem
                >>= findChildByName "DetalleCancelacion"

  case mDetailElem of
    Nothing -> Left
             $ ParseCancelResponseError "Se obtuvo una respuesta sin detalle."

    Just detailElem -> case findChildByName "OperacionExitosa" resElem of
      Nothing -> Left
               $ ParseCancelResponseError "Se obtuvo una respuesta sin estatus."

      Just opStatus -> if getElemText opStatus == "true"
        then do
          case findChildByName "XMLAcuse" resElem of
            Nothing -> Left MissingCancelationAckError

            Just ackElem -> do
              let ack = getElemText ackElem
              if null ack
                 then Left MissingCancelationAckError
                 else Right $ pack ack

        else do
          errMsg <-
            justErr (ParseCancelResponseError "Se obtuvo un error sin mensaje.")
              $ findChildByName "MensajeResultado" detailElem
          Left $ PacCancelError
           (pack . getElemText <$> findChildByName "CodigoResultado" detailElem)
           (pack $ getElemText errMsg)

  where
    mResElem = parseXMLDoc xml
           >>= findChildByName "Body"
           >>= findChildByName "CancelarCFDIResponse"
           >>= findChildByName "CancelarCFDIResult"

felStampResponseParser :: String -> ByteString -> Either StampError PacStamp
felStampResponseParser methodName xml = do
  resElem <- justErr (ParsePacResponseError "Respuesta en formato desconocido.")
           $ mResElem

  case findChildByName "OperacionExitosa" resElem of
    Nothing -> Left
             $ ParsePacResponseError "Se obtuvo una respuesta sin estatus."

    Just opStatus -> if getElemText opStatus == "true"
      then do
        xmlRes <- justErr PacStampNotPresent
                $ findChildByName "XMLResultado" resElem

        case parseCfdiXml (getElemText xmlRes) of
          Left pe -> Left $ ParsePacResponseXMLError pe

          Right cfdi -> justErr PacStampNotPresent $ getStampComplement cfdi

      else do
        errMsg <-
          justErr (ParsePacResponseError "Se obtuvo un error sin mensaje.")
            $ findChildByName "MensajeError" resElem
        Left $ PacError
          (pack $ getElemText errMsg)
          (pack . getElemText <$> findChildByName "CodigoRespuesta" resElem)

  where
    mResElem = parseXMLDoc (decodeUtf8 $ toStrict xml)
           >>= findChildByName "Body"
           >>= findChildByName (methodName ++ "Response")
           >>= findChildByName (methodName ++ "Result")

genFelWsFunc :: Fel -> Text -> XML -> IO ((ByteString -> a) -> IO a)
genFelWsFunc Fel{..} methodName nodes = do
  transport <- initTransportWithM tlsManagerSettings wsUrl pure pure
  return $ invokeWS transport soapAction () body . RawParser
  where
    body = element' methodName $ do
             element' "usuario" felUser
             element' "password" felPass
             nodes
    soapAction = "http://tempuri.org/IWSCFDI33/" ++ unpack methodName
    wsUrl
      | felEnv == FelProductionEnv =
          "https://www.fel.mx/WSTimbrado33/WSCFDI33.svc?WSDL"
      | otherwise = "https://www.fel.mx/WSTimbrado33Test/WSCFDI33.svc?WSDL"

getElemText :: Element -> String
getElemText = maybe "" cdData . listToMaybe . onlyText . elContent
