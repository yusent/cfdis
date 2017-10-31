module CFDI.PAC where

import CFDI.Types   (CFDI(..), Complement(..), PacStamp, complement)
import CFDI.XmlNode (XmlParseError)
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
  deriving (Eq, Show)

stamp :: PAC p => CFDI -> p -> IO (Either StampError CFDI)
stamp cfdi@CFDI { complement = comps } =
  fmap (fmap addStampToCFDI) . getPacStamp cfdi
  where
    addStampToCFDI stamp' = cfdi { complement = StampComplement stamp' : comps }
