module CFDI.Types.StationName where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype StationName = StationName Text deriving (Eq, Show)

instance Chainable StationName where
  chain (StationName n) = n

instance Type StationName where
  parseExpr str
    | matchTest regExp str = Right . StationName $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,50}"
    where
      regExp = mkRegex "^(.|á|é|í|ó|ú|ñ|Á|É|Í|Ó|Ú|Ñ){1,50}$"

  render (StationName n) = unpack n
