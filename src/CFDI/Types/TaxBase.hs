module CFDI.Types.TaxBase where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype TaxBase = TaxBase Text deriving (Eq, Show)

instance Chainable TaxBase where
  chain (TaxBase b) = b

instance Type TaxBase where
  parseExpr str
    | matchTest regExp str = Right . TaxBase $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[0-9]+(.[0-9]{1,6})?"
    where
      regExp = mkRegex "^[0-9]+(\\.[0-9]{1,6})?$"

  render (TaxBase a) = unpack a
