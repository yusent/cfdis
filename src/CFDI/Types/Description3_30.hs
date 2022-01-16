module CFDI.Types.Description3_30 where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype Description3_30 = Description3_30 Text deriving (Eq, Show)

instance Chainable Description3_30 where
  chain (Description3_30 d) = d

instance Type Description3_30 where
  parseExpr str
    | matchTest regExp str = Right . Description3_30 $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{3,30}"
    where
      regExp = mkRegex "^(.|á|é|í|ó|ú|ñ|Á|É|Í|Ó|Ú|Ñ){3,30}$"

  render (Description3_30 d) = unpack d
