module CFDI.Types.Description120 where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype Description120 = Description120 Text deriving (Eq, Show)

instance Chainable Description120 where
  chain (Description120 d) = d

instance Type Description120 where
  parseExpr str
    | matchTest regExp str = Right . Description120 $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,120}"
    where
      regExp = mkRegex "^(.|á|é|í|ó|ú|ñ|Á|É|Í|Ó|Ú|Ñ){1,120}$"

  render (Description120 d) = unpack d
