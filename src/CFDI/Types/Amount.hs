module CFDI.Types.Amount where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (pack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype Amount = Amount Rational deriving (Eq, Show)

instance Chainable Amount where
  chain = pack . render

instance Type Amount where
  parseExpr str
    | matchTest regExp str = Amount <$> parseExpr str
    | otherwise = Left $ DoesNotMatchExpr "[0-9]{1,18}(.[0-9]{1,6})?"
    where
      regExp = mkRegex "^[0-9]{1,18}(\\.[0-9]{1,6})?$"

  -- FIXME: This will only work when using a currency with two decimal places.
  -- At the time of this writing this is not a big deal since Mexico only
  -- supports MXN and USD.
  render (Amount a) = intPart ++ newDecimalPart
    where
      newDecimalPart
        | currentDecimalPlaces > 2 = reverse
                                   . drop (currentDecimalPlaces - 2)
                                   $ reverse currentDecimalPart
        | currentDecimalPlaces < 0 = ".00"
        | currentDecimalPlaces < 2 = currentDecimalPart
                                  ++ replicate (2 - currentDecimalPlaces) '0'
        | otherwise = currentDecimalPart
      (intPart, currentDecimalPart) = break (== '.') $ render a
      currentDecimalPlaces = length currentDecimalPart - 1
