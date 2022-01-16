module CFDI.Types.Description100 where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype Description100 = Description100 Text deriving (Eq, Show)

instance Chainable Description100 where
  chain (Description100 d) = d

instance Type Description100 where
  parseExpr str
    | matchTest regExp str = Right . Description100 $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,100}"
    where
      regExp = mkRegex "^(.|á|é|í|ó|ú|ñ|Á|É|Í|Ó|Ú|Ñ){1,100}$"

  render (Description100 d) = unpack d
