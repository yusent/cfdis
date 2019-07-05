module CFDI.PAC.ITimbre
  ( ITimbre(..)
  , ITimbreEnv(..)
  ) where

import CFDI
  ( PacStamp
  , UUID(..)
  , getStampComplement
  , parseCfdiXml
  , toXML
  )
import CFDI.PAC
import Control.Error.Safe      (justErr)
import Control.Exception       (catch)
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
import Data.Bifunctor          (first)
import Data.ByteString.Lazy    (toStrict)
import Data.Conduit.Attoparsec (errorMessage)
import Data.HashMap.Lazy       (lookup)
import Data.Text               (Text, pack, unpack)
import Data.Vector             (head)
import Network.HTTP.Simple
  ( JSONException(..)
  , Response
  , httpJSONEither
  , getResponseBody
  , getResponseStatusCode
  , setRequestBodyURLEncoded
  )
import Prelude          hiding (head, lookup)

data ITimbre = ITimbre
  { user   :: Text
  , pass   :: Text
  , rfc    :: Text
  , pfxPwd :: Text
  , pfxPem :: Text
  , env    :: ITimbreEnv
  }

data ITimbreCancelResponse = ITimbreCancelResponse Text (Either Text Text)
  deriving (Show)

data ITimbreEnv
  = Production
  | Testing
  deriving (Eq, Show)

data ITimbreResponse = ITimbreResponse Text (Either Text Text)
  deriving (Show)

instance FromJSON ITimbreCancelResponse where
  parseJSON = withObject "ITimbreCancelResponse" $ \v -> do
    retCode    <- (v .: "result") >>= (.: "retcode")
    maybeError <- (v .: "result") >>= (.:? "error")

    let retCode_ = case retCode of
                     Number n -> pack . takeWhile (/= '.') $ show n
                     String t -> t
                     _ -> ""

    case maybeError of
      Just err -> return . ITimbreCancelResponse retCode_ $ Left err

      Nothing  -> ITimbreCancelResponse retCode_ . justErr "No se obtuvo acuse"
                    <$> ((v .: "result") >>= (.:? "acuse"))

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
  cancelCFDI p (UUID uuid) =
    fmap handleITimbreCancelResponse (httpJSONEither request)
      `catch` handleCancelHttpException
    where
      req
        | env p == Production = "POST https://portalws.itimbre.com/itimbre.php"
        | otherwise = "POST https://portalws.itimbre.com/itimbreprueba.php"
      request = setRequestBodyURLEncoded [("q", toStrict $ encode requestBody)]
              $ req
      requestBody = object
        [ "method" .= ("cancelarCFDI" :: Text)
        , "params" .= object
            [ "user"     .= user p
            , "pass"     .= pass p
            , "RFC"      .= rfc p
            , "pfx_pass" .= pfxPwd p
            , "pfx_pem"  .= pfxPem p
            , "folios"   .= [uuid]
            ]
        ]

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

handleITimbreCancelResponse :: Response (Either JSONException Value)
                            -> Either CancelError Text
handleITimbreCancelResponse response
  | responseStatusCode `div` 100 == 2 =
      case responseBody of
        Left (JSONParseException _ _ pe) ->
          Left . ParseCancelResponseError . pack $ errorMessage pe

        Left (JSONConversionException _ _ err) ->
          Left . ParseCancelResponseError $ pack err

        Right body -> case fromJSON body of
          Error err -> Left . ParseCancelResponseError $ pack err

          Success (ITimbreCancelResponse icrRc icrEack) ->
            first (PacCancelError (Just icrRc)) icrEack
  | otherwise = Left . CancelHTTPError responseStatusCode . pack
              $ show responseBody
  where
    responseStatusCode = getResponseStatusCode response
    responseBody = getResponseBody response

handleITimbreResponse :: Response (Either JSONException Value)
                      -> Either StampError PacStamp
handleITimbreResponse response
  | responseStatusCode `div` 100 == 2 =
      case responseBody of
        Left (JSONParseException _ _ pe) ->
          Left . ParsePacResponseError . pack $ errorMessage pe

        Left (JSONConversionException _ _ err) ->
          Left . ParsePacResponseError $ pack err

        Right body -> case fromJSON body of
          Error err -> Left . ParsePacResponseError $ pack err

          Success (ITimbreResponse irrc irc) ->
            case irc of
              Left em -> Left . PacError em $ Just irrc

              Right xml -> case parseCfdiXml (unpack xml) of
                Left pe -> Left $ ParsePacResponseXMLError pe

                Right cfdi ->
                  case getStampComplement cfdi of
                    Nothing -> Left PacStampNotPresent
                    Just stampComp -> Right stampComp

  | otherwise = Left . PacHTTPError responseStatusCode . pack $ show responseBody
  where
    responseStatusCode = getResponseStatusCode response
    responseBody = getResponseBody response

stampRequest :: ITimbreEnv -> Value -> IO (Either StampError PacStamp)
stampRequest e requestBody =
  fmap handleITimbreResponse (httpJSONEither request)
    `catch` handleStampHttpException
  where
    req
      | e == Production = "POST https://portalws.itimbre.com/itimbre.php"
      | otherwise = "POST https://portalws.itimbre.com/itimbreprueba.php"
    request = setRequestBodyURLEncoded [("q", toStrict $ encode requestBody)]
            $ req
