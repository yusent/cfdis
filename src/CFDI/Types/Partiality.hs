module CFDI.Types.Partiality where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (pack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype Partiality = Partiality Int deriving (Eq, Show)

instance Chainable Partiality where
  chain (Partiality p) = pack $ show p

instance Type Partiality where
  parseExpr str
    | matchTest regExp str = Right . Partiality $ read str
    | otherwise = Left $ DoesNotMatchExpr "[1-9][0-9]{0,2}"
    where
      regExp = mkRegex "^[1-9][0-9]{0,2}$"

  render (Partiality p) = show p
