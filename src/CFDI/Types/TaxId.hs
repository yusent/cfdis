module CFDI.Types.TaxId where

import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype TaxId = TaxId Text deriving (Eq, Show)

instance Type TaxId where
  parseExpr str
    | matchTest regExp str = Right . TaxId $ pack str
    | otherwise = Left $ DoesNotMatchExpr ".{1,40}"
    where
      regExp = mkRegex "^.{1,40}$"

  render (TaxId i) = unpack i
