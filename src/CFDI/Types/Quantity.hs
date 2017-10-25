module CFDI.Types.Quantity where

import CFDI.Chainable
import CFDI.Types.Type
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype Quantity = Quantity Rational deriving (Eq, Show)

instance Chainable Quantity where
  chain (Quantity q) = chain q

instance Type Quantity where
  parseExpr str
    | matchTest regExp str = Quantity <$> parseExpr str
    | otherwise = Left $ DoesNotMatchExpr "[0-9]+(.[0-9]{1,6})?"
    where
      regExp = mkRegex "^[0-9]+(\\.[0-9]{1,6})?$"

  render (Quantity q) = render q
