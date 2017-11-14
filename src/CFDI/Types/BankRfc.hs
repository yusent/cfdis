module CFDI.Types.BankRfc where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype BankRfc = BankRfc Text deriving (Eq, Show)

instance Chainable BankRfc where
  chain (BankRfc r) = r

instance Type BankRfc where
  parseExpr str
    | matchTest regExp str = Right . BankRfc $ pack str
    | otherwise = Left
                $ DoesNotMatchExpr
                    "XEXX010101000|[A-Z&Ñ]{3}[0-9]{2}(0[1-9]|1[012])\
                    \(0[1-9]|[12][0-9]|3[01])[A-Z0-9]{2}[0-9A]"
    where
      regExp = mkRegex "^XEXX010101000|[A-Z&Ñ]{3}[0-9]{2}(0[1-9]|1[012])\
                       \(0[1-9]|[12][0-9]|3[01])[A-Z0-9]{2}[0-9A]$"

  render (BankRfc r) = unpack r
