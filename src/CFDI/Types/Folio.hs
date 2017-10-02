module CFDI.Types.Folio where

import CFDI.Types.Type
import Data.Text       (Text, pack, unpack)

data Folio = Folio Text deriving (Eq, Show)

instance Type Folio where
  parseExpr str
    | l > 0 && l < 41 = Right . Folio $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,40}"
    where
      l = length str

  render (Folio f) = unpack f
