module CFDI.PAC
  ( PAC
  , CancelError(..)
  , StampError(..)
  , ValidationError(..)
  , cancelCFDI
  , getPacStamp
  , ppStampError
  , ppValidationError
  , stampLookup
  , stamp
  , stampWithRetry
  ) where

import CFDI                (ppXmlParseError)
import CFDI.Types
  ( CFDI(..)
  , Complement(..)
  , Currency(CUR_MXN)
  , ExchangeRate(..)
  , PacStamp
  , UUID
  , complement
  )
import CFDI.XmlNode        (XmlParseError)
import Data.Maybe          (isJust, isNothing)
import Data.Text           (Text, take, unpack)
import Network.HTTP.Client (HttpExceptionContent)
import Prelude      hiding (take)

class PAC p where
  cancelCFDI :: p -> UUID -> IO (Either CancelError Text)

  getPacStamp :: CFDI -> p -> Text -> IO (Either StampError PacStamp)

  stamp :: CFDI -> p -> IO (Either StampError CFDI)
  stamp cfdi p =
    case validateForStamp cfdi of
      Left vErr -> return . Left $ PreStampValidationError vErr

      Right cfdiId ->
        fmap (fmap (addStampToCFDI cfdi)) $ getPacStamp cfdi p cfdiId

  stampLookup :: p -> Text -> IO (Either StampError PacStamp)

  stampWithRetry :: CFDI -> p -> IO (Either StampError CFDI)
  stampWithRetry cfdi p =
    case validateForStamp cfdi of
      Left vErr -> return . Left $ PreStampValidationError vErr

      Right cfdiId -> do
        eStamp <- getPacStamp cfdi p cfdiId

        fmap (addStampToCFDI cfdi) <$> case eStamp of
          Left (PacError _ (Just "307")) -> stampLookup p cfdiId

          x -> return x

data CancelError
  = PacCancelError
    { pacCancelErrCode :: Maybe Text
    , pacCancelErrMsg :: Text
    }
  | ParseCancelResponseError
    { parseCancelErrMsg :: Text
    }
  | CancelConnectionError
  | CancelHTTPError
    { cancelHTTPCode :: Int
    , cancelHTTPBody :: Text
    }
  deriving (Eq, Show)

data StampError
  = PacConnectionError
    { pacErrExceptionContent :: HttpExceptionContent
    }
  | PacError
    { pacErrMsg  :: Text
    , pacErrCode :: Maybe Text
    }
  | PacHTTPError
    { pacHTTPCode :: Int
    , pacHTTPBody :: Text
    }
  | PacStampNotPresent
  | ParsePacResponseError
    { parsePacErrMsg :: Text
    }
  | ParsePacResponseXMLError
    { parseError :: XmlParseError
    }
  | PreStampValidationError
    { stampValidationErr :: ValidationError
    }
  deriving (Show)

data ValidationError
  = InvalidExchangeRate
  | MissingCerNum
  | MissingCerText
  | MissingSignature
  deriving (Eq, Show)

addStampToCFDI :: CFDI -> PacStamp -> CFDI
addStampToCFDI cfdi@CFDI{ complement = comps } stamp' =
  cfdi { complement = StampComplement stamp' : comps }

ppStampError :: StampError -> String
ppStampError (PacConnectionError _) =
  "No se pudo conectar a servicio de timbrado."
ppStampError (PacError m c) = maybe "" unpack c ++ ": " ++ unpack m
ppStampError (PacHTTPError c b) =
  "Error HTTP código " ++ show c ++ ": " ++ unpack b
ppStampError PacStampNotPresent = "El resultado no contiene un timbre."
ppStampError (ParsePacResponseError e) =
  "No se pudo leer respuesta: " ++ unpack e
ppStampError (ParsePacResponseXMLError e) =
  "No se pudo interpretar XML recibido del PAC:\n" ++ ppXmlParseError "  " e
ppStampError (PreStampValidationError e) = ppValidationError e

ppValidationError :: ValidationError -> String
ppValidationError InvalidExchangeRate = "Tipo de cambio inválido."
ppValidationError MissingCerNum =
  "Se requiere especificar un número de certificado."
ppValidationError MissingCerText = "Se requiere especificar un certificado."
ppValidationError MissingSignature = "Se requiere sellar el CFDI."

validateForStamp :: CFDI -> Either ValidationError Text
validateForStamp cfdi
  | isNothing $ certText cfdi = Left MissingCerText
  | isNothing $ certNum cfdi = Left MissingCerNum
  | isJust xRate && currency cfdi == CUR_MXN && xRate /= Just (ExchangeRate 1) =
      Left InvalidExchangeRate
  | otherwise = case signature cfdi of
                     Nothing -> Left MissingSignature
                     Just sg -> Right $ take 12 sg
  where
    xRate = exchangeRate cfdi
