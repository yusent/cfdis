module CFDI.Types.Series where

import CFDI.Types.Type
import Data.Text       (Text, pack, unpack)

data Series = Series Text deriving (Eq, Show)

instance Type Series where
  parseExpr str
    | l > 0 && l < 26 = Right . Series $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,25}"
    where
      l = length str

  render (Series s) = unpack s
