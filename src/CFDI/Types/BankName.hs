module CFDI.Types.BankName where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype BankName = BankName Text deriving (Eq, Show)

instance Chainable BankName where
  chain (BankName r) = r

instance Type BankName where
  parseExpr str
    | matchTest regExp str = Right . BankName $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,300}"
    where
      regExp = mkRegex "^.{1,300}$"

  render (BankName r) = unpack r
