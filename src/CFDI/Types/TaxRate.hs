module CFDI.Types.TaxRate where

import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

data TaxRate = TaxRate Text deriving (Eq, Show)

instance Type TaxRate where
  parseExpr str
    | matchTest regExp str = Right . TaxRate $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[0-9]+(.[0-9]{1,6})?"
    where
      regExp = mkRegex "^[0-9]+(\\.[0-9]{1,6})?$"

  render (TaxRate r) = unpack r
