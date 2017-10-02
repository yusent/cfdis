module CFDI.Types.Folio where

import CFDI.Types.Type
import Data.Text       (Text, pack, unpack)

data Folio = Folio Text deriving (Eq, Show)

instance Type Folio where
  parse str
    | invalidLength || wrongFormat   = Left $ DoesNotMatchExpr "[^|]{1,40}"
    | otherwise = Right . Folio $ pack str
    where
      invalidLength = l < 1 || l > 40
      l = length str
      wrongFormat = any (== '|') str

  render (Folio f) = unpack f
