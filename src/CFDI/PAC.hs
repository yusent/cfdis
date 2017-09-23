module CFDI.PAC where

import CFDI.Types    (CFDI, Complement(..), PacStamp, complement, pacStamp)
import CFDI.Renderer (toXML)
import Data.Maybe    (fromMaybe)
import Data.Text     (Text)

type XML = Text

class PAC p where
  getPacStamp :: XML -> p -> IO (Either StampError PacStamp)

data StampError = StampError
  { stampErrMsg  :: Text
  , stampErrCode :: Maybe Text
  }

stamp :: PAC p => CFDI -> p -> IO (Either StampError CFDI)
stamp cfdi =
  fmap (fmap addStampToCFDI) . getPacStamp (toXML cfdi)
  where
    addStampToCFDI stamp' =
      cfdi { complement = Just (complement' { pacStamp = Just stamp' }) }
    complement' = fromMaybe (Complement Nothing) $ complement cfdi
