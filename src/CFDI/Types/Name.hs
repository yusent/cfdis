module CFDI.Types.Name where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype Name = Name Text deriving (Eq, Show)

instance Chainable Name where
  chain (Name n) = n

instance Type Name where
  parseExpr str
    | matchTest regExp str = Right . Name $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,254}"
    where
      regExp = mkRegex "^.{1,254}$"

  render (Name n) = unpack n
