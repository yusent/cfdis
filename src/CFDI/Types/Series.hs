module CFDI.Types.Series where

import CFDI.Types.Type
import Data.Text       (Text, pack, unpack)

data Series = Series Text deriving (Eq, Show)

instance Type Series where
  parse str
    | invalidLength || wrongFormat   = Left $ DoesNotMatchExpr "[^|]{1,25}"
    | otherwise = Right . Series $ pack str
    where
      invalidLength = l < 1 || l > 25
      l = length str
      wrongFormat = any (== '|') str

  render (Series s) = unpack s
