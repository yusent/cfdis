module CFDI.Types.Description7_30 where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype Description7_30 = Description7_30 Text deriving (Eq, Show)

instance Chainable Description7_30 where
  chain (Description7_30 d) = d

instance Type Description7_30 where
  parseExpr str
    | matchTest regExp str = Right . Description7_30 $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{7,30}"
    where
      regExp = mkRegex "^(.|á|é|í|ó|ú|ñ|Á|É|Í|Ó|Ú|Ñ){7,30}$"

  render (Description7_30 d) = unpack d
