module CFDI.Types.Folio where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype Folio = Folio Text deriving (Eq, Show)

instance Chainable Folio where
  chain (Folio f) = f

instance Type Folio where
  parseExpr str
    | matchTest regExp str = Right . Folio $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,40}"
    where
      regExp = mkRegex "^.{1,40}$"

  render (Folio f) = unpack f
