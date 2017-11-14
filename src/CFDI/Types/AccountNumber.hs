module CFDI.Types.AccountNumber where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype AccountNumber = AccountNumber Text deriving (Eq, Show)

instance Chainable AccountNumber where
  chain (AccountNumber n) = n

instance Type AccountNumber where
  parseExpr str
    | matchTest regExp str = Right . AccountNumber $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[A-Z0-9_]{10,50}"
    where
      regExp = mkRegex "^[A-Z0-9_]{10,50}$"

  render (AccountNumber n) = unpack n
