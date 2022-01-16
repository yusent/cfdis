module CFDI.Types.KGWeight where

import CFDI.Chainable
import CFDI.Types.Type
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype KGWeight = KGWeight Rational deriving (Eq, Show)

instance Chainable KGWeight where
  chain (KGWeight w) = chain w

instance Type KGWeight where
  parseExpr str
    | matchTest regExp str = KGWeight <$> parseExpr str
    | otherwise = Left $ DoesNotMatchExpr "[0-9]+(.[0-9]{1,3})?"
    where
      regExp = mkRegex "^[0-9]+(\\.[0-9]{1,3})?$"

  render (KGWeight w) = render w
