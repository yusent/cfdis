module CFDI.Types.Confirmation where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype Confirmation = Confirmation Text deriving (Eq, Show)

instance Chainable Confirmation where
  chain (Confirmation c) = c

instance Type Confirmation where
  parseExpr str
    | matchTest regExp str = Right . Confirmation $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[0-9a-zA-Z]{5}"
    where
      regExp = mkRegex "^[0-9a-zA-Z]{5}$"

  render (Confirmation c) = unpack c
