module CFDI.Types.ExchangeRate where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype ExchangeRate = ExchangeRate Text deriving (Eq, Show)

instance Chainable ExchangeRate where
  chain (ExchangeRate r) = r

instance Type ExchangeRate where
  parseExpr str
    | matchTest regExp str = Right . ExchangeRate $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[0-9]+(.[0-9]{1,6})?"
    where
      regExp = mkRegex "^[0-9]+(\\.[0-9]{1,6})?$"

  render (ExchangeRate a) = unpack a
