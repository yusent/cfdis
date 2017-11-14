module CFDI.Types.OperationId where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype OperationId = OperationId Text deriving (Eq, Show)

instance Chainable OperationId where
  chain (OperationId i) = i

instance Type OperationId where
  parseExpr str
    | matchTest regExp str = Right . OperationId $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,100}"
    where
      regExp = mkRegex "^.{1,100}$"

  render (OperationId i) = unpack i
