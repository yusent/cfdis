module CFDI.Types.Description3_50 where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype Description3_50 = Description3_50 Text deriving (Eq, Show)

instance Chainable Description3_50 where
  chain (Description3_50 d) = d

instance Type Description3_50 where
  parseExpr str
    | matchTest regExp str = Right . Description3_50 $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{3,50}"
    where
      regExp = mkRegex "^(.|á|é|í|ó|ú|ñ|Á|É|Í|Ó|Ú|Ñ){3,50}$"

  render (Description3_50 d) = unpack d
