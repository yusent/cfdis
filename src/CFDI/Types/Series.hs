module CFDI.Types.Series where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype Series = Series Text deriving (Eq, Show)

instance Chainable Series where
  chain (Series s) = s

instance Type Series where
  parseExpr str
    | matchTest regExp str = Right . Series $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,25}"
    where
      regExp = mkRegex "^.{1,25}$"

  render (Series s) = unpack s
