module CFDI.Types.ProductDescription where

import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

data ProductDescription = ProductDescription Text deriving (Eq, Show)

instance Type ProductDescription where
  parseExpr str
    | matchTest regExp str = Right . ProductDescription $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,1000}"
    where
      regExp = mkRegex "^.{1,1000}$"

  render (ProductDescription d) = unpack d
