module CFDI.Types.PaymentChain where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype PaymentChain = PaymentChain Text deriving (Eq, Show)

instance Chainable PaymentChain where
  chain (PaymentChain c) = c

instance Type PaymentChain where
  parseExpr str
    | matchTest regExp str = Right . PaymentChain $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,8192}"
    where
      regExp = mkRegex "^.{1,8192}$"

  render (PaymentChain c) = unpack c
