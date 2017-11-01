module CFDI.PAC where

import CFDI.Types
  ( CFDI(..)
  , Complement(..)
  , Currency(CUR_MXN)
  , ExchangeRate(..)
  , PacStamp
  , complement
  )
import CFDI.XmlNode (XmlParseError)
import Data.Maybe   (isJust, isNothing)
import Data.Text    (Text)

class PAC p where
  getPacStamp :: CFDI -> p -> IO (Either StampError PacStamp)

data StampError
  = PacConnectionError
    { connErrMsg :: Text
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
  deriving (Eq, Show)

data ValidationError
  = InvalidExchangeRate
  | MissingCerNum
  | MissingCerText
  | MissingSignature
  deriving (Eq, Show)

stamp :: PAC p => CFDI -> p -> IO (Either StampError CFDI)
stamp cfdi@CFDI { complement = comps } p =
  case validate cfdi of
    Just vErr -> return . Left $ PreStampValidationError vErr

    Nothing -> fmap (fmap addStampToCFDI) $ getPacStamp cfdi p
  where
    addStampToCFDI stamp' = cfdi { complement = StampComplement stamp' : comps }

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
