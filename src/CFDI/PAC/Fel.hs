{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CFDI.PAC.Fel where

import CFDI
  ( PacStamp
  , UUID(..)
  , getStampComplement
  , pacStamp
  , parseCfdiXml
  , toXML
  )
import CFDI.PAC
import CFDI.XmlNode
import Control.Error.Safe          (justErr)
import Data.ByteString.Lazy        (ByteString)
import Data.Maybe                  (listToMaybe)
import Data.Text                   (Text, pack, unpack)
import Network.HTTP.Client
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
  cancelCFDI Fel{..} (UUID _uuid) =
    return . Left $ ParseCancelResponseError "NO IMPLEMENTADO"

  getPacStamp cfdi fel cfdiId = do
    f <- genFelWsFunc fel "TimbrarCFDI" $ do
      element' "cadenaXML" . pack $ toXML cfdi
      element' "referencia" cfdiId
    f felResponseParser

  stampLookup Fel{..} _cfdiId = return . Left $ PacError "NO IMPLEMENTADO" Nothing

element' :: ToXML a => Text -> a -> XML
element' n = element (Name n (Just "http://tempuri.org/") (Just "tem"))

extractResultElement :: ByteString -> Maybe Element
extractResultElement xml = parseXMLDoc xml
                       >>= findChildByName "Body"
                       >>= findChildByName "TimbrarCFDIResponse"
                       >>= findChildByName "TimbrarCFDIResult"

felResponseParser :: ByteString -> Either StampError PacStamp
felResponseParser xml = do
  resElem <- justErr (ParsePacResponseError "Respuesta en formato desconocido.")
           $ extractResultElement xml

  case findChildByName "OperacionExitosa" resElem of
    Nothing -> Left
             $ ParsePacResponseError "Se obtuvo una respuesta sin estatus."

    Just opStatus -> if getElemText opStatus == "true"
      then do
        xmlRes <- justErr PacStampNotPresent
                $ findChildByName "XMLResultado" resElem

        case parseCfdiXml (getElemText xmlRes) of
          Left pe -> Left $ ParsePacResponseXMLError pe

          Right cfdi -> do
            stampComp <- justErr PacStampNotPresent $ getStampComplement cfdi
            return $ pacStamp stampComp

      else do
        errMsg <-
          justErr (ParsePacResponseError "Se obtuvo un error sin mensaje.")
            $ findChildByName "MensajeError" resElem
        Left $ PacError
          (pack $ getElemText errMsg)
          (pack . getElemText <$> findChildByName "CodigoRespuesta" resElem)

genFelWsFunc :: Fel -> Text -> XML -> IO ((ByteString -> a) -> IO a)
genFelWsFunc Fel{..} methodName nodes = do
  transport <- initTransportWithM defaultManagerSettings wsUrl pure pure
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
      | otherwise = "http://www.fel.mx/WSTimbrado33Test/WSCFDI33.svc?WSDL"

getElemText :: Element -> String
getElemText = maybe "" cdData . listToMaybe . onlyText . elContent
