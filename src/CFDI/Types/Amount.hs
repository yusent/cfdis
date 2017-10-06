module CFDI.Types.Amount where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype Amount = Amount Text deriving (Eq, Show)

instance Chainable Amount where
  chain (Amount a) = a

instance Type Amount where
  parseExpr str
    | matchTest regExp str = Right . Amount $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[0-9]{1,18}(.[0-9]{1,6})?"
    where
      regExp = mkRegex "^[0-9]{1,18}(\\.[0-9]{1,6})?$"

  render (Amount a) = unpack a
