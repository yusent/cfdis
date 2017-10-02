module CFDI.Types.ExchangeRate where

import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

data ExchangeRate = ExchangeRate Text deriving (Eq, Show)

instance Type ExchangeRate where
  parseExpr str
    | matchTest regExp str = Right . ExchangeRate $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[0-9]+(.[0-9]{1,6})?"
    where
      regExp = mkRegex "^[0-9]+(\\.[0-9]{1,6})?$"

  render (ExchangeRate a) = unpack a
