module CFDI.Types.ExchangeRate where

import CFDI.Chainable
import CFDI.Types.Type
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype ExchangeRate = ExchangeRate Rational deriving (Eq, Show)

instance Chainable ExchangeRate where
  chain (ExchangeRate r) = chain r

instance Type ExchangeRate where
  parseExpr str
    | matchTest regExp str = ExchangeRate <$> parseExpr str
    | otherwise = Left $ DoesNotMatchExpr "[0-9]+(.[0-9]{1,6})?"
    where
      regExp = mkRegex "^[0-9]+(\\.[0-9]{1,6})?$"

  render (ExchangeRate a) = render a
