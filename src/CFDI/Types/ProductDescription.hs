module CFDI.Types.ProductDescription where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype ProductDescription = ProductDescription Text deriving (Eq, Show)

instance Chainable ProductDescription where
  chain (ProductDescription d) = d

instance Type ProductDescription where
  parseExpr str
    | matchTest regExp str = Right . ProductDescription $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,1000}"
    where
      regExp = mkRegex "^(.|á|é|í|ó|ú|ñ|Á|É|Í|Ó|Ú|Ñ){1,1000}$"

  render (ProductDescription d) = unpack d
