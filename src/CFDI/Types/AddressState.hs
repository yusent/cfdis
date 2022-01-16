module CFDI.Types.AddressState where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype AddressState = AddressState Text deriving (Eq, Show)

instance Chainable AddressState where
  chain (AddressState s) = s

instance Type AddressState where
  parseExpr str
    | matchTest regExp str = Right . AddressState $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,30}"
    where
      regExp = mkRegex "^(.|á|é|í|ó|ú|ñ|Á|É|Í|Ó|Ú|Ñ){1,30}$"

  render (AddressState s) = unpack s
