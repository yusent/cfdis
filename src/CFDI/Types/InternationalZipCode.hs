module CFDI.Types.InternationalZipCode where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype InternationalZipCode = InternationalZipCode Text deriving (Eq, Show)

instance Chainable InternationalZipCode where
  chain (InternationalZipCode c) = c

instance Type InternationalZipCode where
  parseExpr str
    | matchTest regExp str = Right . InternationalZipCode $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,12}"
    where
      regExp = mkRegex "^(.|á|é|í|ó|ú|ñ|Á|É|Í|Ó|Ú|Ñ){1,12}$"

  render (InternationalZipCode c) = unpack c
