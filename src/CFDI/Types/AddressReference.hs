module CFDI.Types.AddressReference where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype AddressReference = AddressReference Text deriving (Eq, Show)

instance Chainable AddressReference where
  chain (AddressReference r) = r

instance Type AddressReference where
  parseExpr str
    | matchTest regExp str = Right . AddressReference $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,250}"
    where
      regExp = mkRegex "^(.|á|é|í|ó|ú|ñ|Á|É|Í|Ó|Ú|Ñ){1,250}$"

  render (AddressReference r) = unpack r
