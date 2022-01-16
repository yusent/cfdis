module CFDI.Types.AddressNumber where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype AddressNumber = AddressNumber Text deriving (Eq, Show)

instance Chainable AddressNumber where
  chain (AddressNumber n) = n

instance Type AddressNumber where
  parseExpr str
    | matchTest regExp str = Right . AddressNumber $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,55}"
    where
      regExp = mkRegex "^(.|á|é|í|ó|ú|ñ|Á|É|Í|Ó|Ú|Ñ){1,55}$"

  render (AddressNumber n) = unpack n
