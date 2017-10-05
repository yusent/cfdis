module CFDI.Types.RFC where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype RFC = RFC Text deriving (Eq, Show)

instance Chainable RFC where
  chain (RFC r) = r

instance Type RFC where
  parseExpr str
    | matchTest regExp str = Right . RFC $ pack str
    | otherwise = Left
                $ DoesNotMatchExpr "[A-Z&Ñ]{3,4}[0-9]{2}(0[1-9]|1[012])(0[1-9]|\
                                   \[12][0-9]|3[01])[A-Z0-9]{2}[0-9A]"
    where
      regExp = mkRegex "^[A-Z&Ñ]{3,4}[0-9]{2}(0[1-9]|1[012])(0[1-9]|[12][0-9]|3\
                       \[01])[A-Z0-9]{2}[0-9A]$"

  render (RFC r) = unpack r
