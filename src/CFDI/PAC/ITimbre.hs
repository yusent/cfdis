module CFDI.PAC.ITimbre
  ( ITimbre(..)
  , ITimbreEnv(..)
  ) where

import CFDI
  ( CFDI(..)
  , Complement(StampComplement)
  , PacStamp
  , complement
  , pacStamp
  , parseCfdiXml
  , signature
  , toXML
  )
import CFDI.PAC
import Control.Error.Safe        (justErr)
import Control.Exception         (catch, throw)
import Data.Aeson
  ( FromJSON
  , Result(Success, Error)
  , Value(Number, String)
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
import Data.List                 (find)
import Data.Text                 (Text, pack, take, unpack)
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

    let retCode_ = case retCode of
                     Number n -> pack . takeWhile (/= '.') $ show n
                     String t -> t
                     _ -> ""

    case maybeError of
      Just err -> return . ITimbreResponse retCode_ $ Left err
      Nothing  -> ITimbreResponse retCode_ . justErr "No se obtuvo XML"
        <$> ((v .: "result") >>= (.:? "data"))

instance PAC ITimbre where
  getPacStamp cfdi@CFDI{ signature = Just sig } p = do
    fmap handleITimbreResponse (httpJSON request) `catch` handleHttpException
    where
      req
        | env p == Production = "POST https://portalws.itimbre.com/itimbre.php"
        | otherwise = "POST https://portalws.itimbre.com/itimbreprueba.php"
      request = setRequestBodyURLEncoded [("q", toStrict $ encode requestBody)]
              $ req
      requestBody = object
        [ "id"     .= take 12 sig
        , "method" .= ("cfd2cfdi" :: Text)
        , "params" .= object
            [ "user"    .= user p
            , "pass"    .= pass p
            , "RFC"     .= rfc p
            , "xmldata" .= toXML cfdi
            ]
        ]

  getPacStamp _ _ = return $ Left UncaughtValidationError

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

            Right xml -> case parseCfdiXml (unpack xml) of
              Left pe -> Left $ ParsePacResponseXMLError pe

              Right CFDI { complement = comps } ->
                case find isStampComplement comps of
                  Nothing -> Left PacStampNotPresent

                  Just stampComp -> Right $ pacStamp stampComp

  | otherwise = Left . PacHTTPError responseStatusCode . pack $ show responseBody
  where
    isStampComplement (StampComplement _) = True
    isStampComplement _ = False
    responseStatusCode = getResponseStatusCode response
    responseBody = getResponseBody response
