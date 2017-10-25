module CFDI.Types.TaxRate where

import CFDI.Chainable
import CFDI.Types.Type
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype TaxRate = TaxRate Rational deriving (Eq, Show)

instance Chainable TaxRate where
  chain (TaxRate r) = chain r

instance Type TaxRate where
  parseExpr str
    | matchTest regExp str = TaxRate <$> parseExpr str
    | otherwise = Left $ DoesNotMatchExpr "[0-9]+(.[0-9]{1,6})?"
    where
      regExp = mkRegex "^[0-9]+(\\.[0-9]{1,6})?$"

  render (TaxRate r) = render r
