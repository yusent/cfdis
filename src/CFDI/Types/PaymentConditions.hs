module CFDI.Types.PaymentConditions where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype PaymentConditions = PaymentConditions Text deriving (Eq, Show)

instance Chainable PaymentConditions where
  chain (PaymentConditions c) = c

instance Type PaymentConditions where
  parseExpr str
    | matchTest regExp str = Right . PaymentConditions $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,1000}"
    where
      regExp = mkRegex "^.{1,1000}$"

  render (PaymentConditions pc) = unpack pc
