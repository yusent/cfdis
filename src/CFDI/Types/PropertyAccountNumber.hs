module CFDI.Types.PropertyAccountNumber where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype PropertyAccountNumber = PropertyAccountNumber Text deriving (Eq, Show)

instance Chainable PropertyAccountNumber where
  chain (PropertyAccountNumber n) = n

instance Type PropertyAccountNumber where
  parseExpr str
    | matchTest regExp str = Right . PropertyAccountNumber $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[0-9]{1,150}"
    where
      regExp = mkRegex "^[0-9]{1,150}$"

  render (PropertyAccountNumber n) = unpack n
