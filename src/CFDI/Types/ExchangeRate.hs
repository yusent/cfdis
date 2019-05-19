module CFDI.Types.ExchangeRate where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype ExchangeRate = ExchangeRate Rational deriving (Eq, Show)

instance Chainable ExchangeRate where
  chain (ExchangeRate r) = pack . addZeros . unpack $ chain r

instance Type ExchangeRate where
  parseExpr str
    | matchTest regExp str = ExchangeRate <$> parseExpr str
    | otherwise = Left $ DoesNotMatchExpr "[0-9]+(.[0-9]{1,6})?"
    where
      regExp = mkRegex "^[0-9]+(\\.[0-9]{1,6})?$"

  render (ExchangeRate a) = addZeros $ render a

addZeros :: String -> String
addZeros s
  | dec == "" = int ++ ".000000"
  | otherwise = int ++ dec ++ replicate (5 - length dec) '0'
  where
    (int, dec) = span (/= '.') s
