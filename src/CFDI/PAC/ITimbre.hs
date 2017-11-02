module CFDI.PAC.ITimbre
  ( ITimbre(..)
  , ITimbreEnv(..)
  ) where

import CFDI
  ( PacStamp
  , getStampComplement
  , pacStamp
  , parseCfdiXml
  , toXML
  )
import CFDI.PAC
import Control.Error.Safe        (justErr)
import Control.Exception         (catch, throw)
import Data.Aeson
  ( FromJSON
  , Result(Success, Error)
  , Value(Array, Number, Object, String)
  , (.=)
  , (.:)
  , (.:?)
  , encode
  , fromJSON
  , object
  , parseJSON
  , withObject
  )
import Data.ByteString.Lazy      (toStrict)
import Data.HashMap.Lazy         (lookup)
import Data.Text                 (Text, pack, unpack)
import Data.Text.Encoding        (decodeUtf8)
import Data.Vector               (head)
import Network.HTTP.Conduit
  ( HttpException(HttpExceptionRequest)
  , HttpExceptionContent(StatusCodeException)
  , responseStatus
  )
import Network.HTTP.Simple
  ( Response
  , httpJSON
  , getResponseBody
  , getResponseStatusCode
  , setRequestBodyURLEncoded
  )
import Network.HTTP.Types.Status (statusCode)
import Prelude            hiding (head, lookup)

data ITimbre = ITimbre
  { user :: Text
  , pass :: Text
  , rfc  :: Text
  , env  :: ITimbreEnv
  }

data ITimbreEnv
  = Production
  | Testing
  deriving (Eq, Show)

data ITimbreResponse = ITimbreResponse
  { irRetCode :: Text
  , irContent :: Either Text Text
  }
  deriving (Show)

instance FromJSON ITimbreResponse where
  parseJSON = withObject "ITimbreResponse" $ \v -> do
    retCode    <- (v .: "result") >>= (.: "retcode")
    maybeError <- (v .: "result") >>= (.:? "error")

    let retCode_ = case retCode of
                     Number n -> pack . takeWhile (/= '.') $ show n
                     String t -> t
                     _ -> ""

    case maybeError of
      Just err -> return . ITimbreResponse retCode_ $ Left err

      Nothing  -> do
        mData <- (v .: "result") >>= (.:? "data")

        return . ITimbreResponse retCode_ . justErr "No se obtuvo XML"
          $ case mData of
              Just (String data_) -> Just data_

              Just (Array vec) -> case head vec of
                Object o -> case lookup "xml_data" o of
                  Just (String xml) -> Just xml

                  _ -> Nothing

                _ -> Nothing

              _ -> Nothing

instance PAC ITimbre where
  getPacStamp cfdi p cfdiId =
    stampRequest (env p) $ object
      [ "id"     .= cfdiId
      , "method" .= ("cfd2cfdi" :: Text)
      , "params" .= object
          [ "user"    .= user p
          , "pass"    .= pass p
          , "RFC"     .= rfc p
          , "xmldata" .= toXML cfdi
          ]
      ]

  stampLookup p cfdiId = do
    stampRequest (env p) $ object
      [ "id"     .= cfdiId
      , "method" .= ("buscarCFDI" :: Text)
      , "params" .= object
          [ "user"    .= user p
          , "pass"    .= pass p
          , "RFC"     .= rfc p
          ]
      ]

handleHttpException :: HttpException -> IO (Either StampError PacStamp)
handleHttpException (HttpExceptionRequest _ (StatusCodeException res body)) =
  return . Left . PacHTTPError status $ decodeUtf8 body
  where
    status = statusCode $ responseStatus res
handleHttpException (HttpExceptionRequest _ e) =
  return . Left $ PacConnectionError e
handleHttpException e = throw e

handleITimbreResponse :: Response Value -> Either StampError PacStamp
handleITimbreResponse response
  | responseStatusCode `div` 100 == 2 =
      case fromJSON responseBody of
        Error err -> Left $ ParsePacResponseError $ pack err

        Success (ITimbreResponse { irRetCode = irrc, irContent = irc }) ->
          case irc of
            Left em -> Left . PacError em $ Just irrc

            Right xml -> case parseCfdiXml (unpack xml) of
              Left pe -> Left $ ParsePacResponseXMLError pe

              Right cfdi ->
                case getStampComplement cfdi of
                  Nothing -> Left PacStampNotPresent

                  Just stampComp -> Right $ pacStamp stampComp

  | otherwise = Left . PacHTTPError responseStatusCode . pack $ show responseBody
  where
    responseStatusCode = getResponseStatusCode response
    responseBody = getResponseBody response

stampRequest :: ITimbreEnv -> Value -> IO (Either StampError PacStamp)
stampRequest e requestBody =
  fmap handleITimbreResponse (httpJSON request) `catch` handleHttpException
  where
    req
      | e == Production = "POST https://portalws.itimbre.com/itimbre.php"
      | otherwise = "POST https://portalws.itimbre.com/itimbreprueba.php"
    request = setRequestBodyURLEncoded [("q", toStrict $ encode requestBody)]
            $ req
