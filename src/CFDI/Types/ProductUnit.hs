module CFDI.Types.ProductUnit where

import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

data ProductUnit = ProductUnit Text deriving (Eq, Show)

instance Type ProductUnit where
  parseExpr str
    | matchTest regExp str = Right . ProductUnit $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,20}"
    where
      regExp = mkRegex "^.{1,20}$"

  render (ProductUnit u) = unpack u
