module CFDI.Types.Description50 where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype Description50 = Description50 Text deriving (Eq, Show)

instance Chainable Description50 where
  chain (Description50 d) = d

instance Type Description50 where
  parseExpr str
    | matchTest regExp str = Right . Description50 $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,50}"
    where
      regExp = mkRegex "^(.|á|é|í|ó|ú|ñ|Á|É|Í|Ó|Ú|Ñ){1,50}$"

  render (Description50 d) = unpack d
