module CFDI.Types.Confirmation where

import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

data Confirmation = Confirmation Text deriving (Eq, Show)

instance Type Confirmation where
  parseExpr str
    | matchTest regExp str = Right . Confirmation $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[0-9a-zA-Z]{5}"
    where
      regExp = mkRegex "^[0-9a-zA-Z]{5}$"

  render (Confirmation c) = unpack c
