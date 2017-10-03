module CFDI.Types.Folio where

import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

data Folio = Folio Text deriving (Eq, Show)

instance Type Folio where
  parseExpr str
    | matchTest regExp str = Right . Folio $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,40}"
    where
      regExp = mkRegex "^.{1,40}$"

  render (Folio f) = unpack f
