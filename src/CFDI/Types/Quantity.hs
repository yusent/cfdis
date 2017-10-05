module CFDI.Types.Quantity where

import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype Quantity = Quantity Text deriving (Eq, Show)

instance Type Quantity where
  parseExpr str
    | matchTest regExp str = Right . Quantity $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[0-9]+(.[0-9]{1,6})?"
    where
      regExp = mkRegex "^[0-9]+(\\.[0-9]{1,6})?$"

  render (Quantity q) = unpack q
