module CFDI.PAC
  ( PAC
  , StampError(..)
  , ValidationError(..)
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
  , complement
  )
import CFDI.XmlNode        (XmlParseError)
import Data.Maybe          (isJust, isNothing)
import Data.Text           (Text, unpack)
import Network.HTTP.Client (HttpExceptionContent)

class PAC p where
  getPacStamp :: CFDI -> p -> IO (Either StampError PacStamp)

  stamp :: CFDI -> p -> IO (Either StampError CFDI)
  stamp cfdi p =
    case validate cfdi of
      Just vErr -> return . Left $ PreStampValidationError vErr

      Nothing -> fmap (fmap (addStampToCFDI cfdi)) $ getPacStamp cfdi p

  stampLookup :: CFDI -> p -> IO (Either StampError PacStamp)

  stampWithRetry :: CFDI -> p -> IO (Either StampError CFDI)
  stampWithRetry cfdi p =
    case validate cfdi of
      Just vErr -> return . Left $ PreStampValidationError vErr

      Nothing -> do
        eStamp <- getPacStamp cfdi p

        fmap (addStampToCFDI cfdi) <$> case eStamp of
          Left (PacError _ (Just "307")) -> stampLookup cfdi p

          x -> return x

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
  | UncaughtValidationError
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
ppStampError UncaughtValidationError = "Error de validación inesperado."

ppValidationError :: ValidationError -> String
ppValidationError InvalidExchangeRate = "Tipo de cambio inválido."
ppValidationError MissingCerNum =
  "Se requiere especificar un número de certificado."
ppValidationError MissingCerText = "Se requiere especificar un certificado."
ppValidationError MissingSignature = "Se requiere sellar el CFDI."

validate :: CFDI -> Maybe ValidationError
validate cfdi
  | isNothing $ certText cfdi = Just MissingCerText
  | isNothing $ certNum cfdi = Just MissingCerNum
  | isNothing $ signature cfdi = Just MissingSignature
  | isJust xRate && currency cfdi == CUR_MXN && xRate /= Just (ExchangeRate 1) =
      Just InvalidExchangeRate
  | otherwise = Nothing
  where
    xRate = exchangeRate cfdi
