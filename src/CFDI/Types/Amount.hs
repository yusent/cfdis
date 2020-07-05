module CFDI.Types.Amount where

import CFDI.Chainable
import CFDI.Types.Type
import Data.List        (intercalate)
import Data.Text        (Text, pack)
import Numeric          (fromRat, showFFloatAlt)
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
  -- Note: Payment CFDIs required to have amounts of exactly "0", "0.0" is
  -- considered to be invalid :(.
  render (Amount 0) = "0"
  render (Amount r) = showFFloatAlt (Just 2) (fromRat r :: Double) ""

formatAmount :: Amount -> Text
formatAmount amount = pack $ commaSeparatedIntPart ++ decimalPart
  where
    commaSeparatedIntPart = reverse
                          . intercalate ","
                          . splitEvery3
                          $ reverse intPart
    (intPart, decimalPart) = break (== '.') $ render amount
    splitEvery3 (a : b : c : rest) = [a, b, c] : splitEvery3 rest
    splitEvery3 [] = []
    splitEvery3 x = [x]
