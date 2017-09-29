{-# LANGUAGE OverloadedStrings #-}

module CFDI.V3_2.PAC.ITimbre
  ( ITimbre(..)
  , ITimbreEnv(..)
  ) where

import CFDI.V3_2.PAC
import CFDI.V3_2.Parser          (parse)
import CFDI.V3_2.Renderer        (toXML)
import CFDI.V3_2.Types
  ( PacStamp
  , complement
  , pacStamp
  , signature
  )
import Control.Error.Safe        (justErr)
import Control.Exception         (catch, throw)
import Data.Aeson
  ( FromJSON
  , Result(Success, Error)
  , Value
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
import Data.Text                 (Text, pack, take)
import Data.Text.Encoding        (decodeUtf8)
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
import Prelude            hiding (take)

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

    let retCode_ = pack . show $ (retCode :: Int)

    case maybeError of
      Just err -> return . ITimbreResponse retCode_ $ Left err
      Nothing  -> ITimbreResponse retCode_ . justErr "No se obtuvo XML"
        <$> ((v .: "result") >>= (.:? "data"))

instance PAC ITimbre where
  getPacStamp cfdi p = do
    fmap handleITimbreResponse (httpJSON request) `catch` handleHttpException
    where
      req
        | env p == Production = "POST https://portalws.itimbre.com/itimbre.php"
        | otherwise = "POST https://portalws.itimbre.com/itimbreprueba.php"
      request = setRequestBodyURLEncoded [("q", toStrict $ encode requestBody)]
              $ req
      requestBody = object
        [ "id"     .= take 12 (signature cfdi)
        , "method" .= ("cfd2cfdi" :: Text)
        , "params" .= object
            [ "user"    .= user p
            , "pass"    .= pass p
            , "RFC"     .= rfc p
            , "xmldata" .= toXML cfdi
            ]
        ]

handleHttpException :: HttpException -> IO (Either StampError PacStamp)
handleHttpException (HttpExceptionRequest _ (StatusCodeException res body)) =
  return . Left . PacHTTPError status $ decodeUtf8 body
  where
    status = statusCode $ responseStatus res
handleHttpException e = throw e

handleITimbreResponse :: Response Value -> Either StampError PacStamp
handleITimbreResponse response
  | responseStatusCode `div` 100 == 2 =
      case fromJSON responseBody of
        Error err -> Left $ ParsePacResponseError $ pack err

        Success (ITimbreResponse { irRetCode = irrc, irContent = irc }) ->
          case irc of
            Left em -> Left . PacError em $ Just irrc

            Right xml -> case parse xml of
              Left pe -> Left $ ParsePacResponseXMLError pe

              Right cfdi_ -> case complement cfdi_ of
                Nothing -> Left PacStampNotPresent

                Just comp -> case pacStamp comp of
                  Nothing -> Left PacStampNotPresent

                  Just ps -> Right ps

  | otherwise = Left . PacHTTPError responseStatusCode . pack $ show responseBody
  where
    responseStatusCode = getResponseStatusCode response
    responseBody = getResponseBody response
