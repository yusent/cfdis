module CFDI.Types.CompanyRfc where

import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

data CompanyRfc = CompanyRfc Text deriving (Eq, Show)

instance Type CompanyRfc where
  parseExpr str
    | matchTest regExp str = Right . CompanyRfc $ pack str
    | otherwise = Left
                $ DoesNotMatchExpr "[A-Z&Ñ]{3}[0-9]{2}(0[1-9]|1[012])(0[1-9]|\
                                   \[12][0-9]|3[01])[A-Z0-9]{2}[0-9A]"
    where
      regExp = mkRegex "^[A-Z&Ñ]{3}[0-9]{2}(0[1-9]|1[012])(0[1-9]|[12][0-9]|3\
                       \[01])[A-Z0-9]{2}[0-9A]$"

  render (CompanyRfc r) = unpack r
