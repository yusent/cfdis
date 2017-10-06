module CFDI.Types.CertificateNumber where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype CertificateNumber = CertificateNumber Text deriving (Eq, Show)

instance Chainable CertificateNumber where
  chain (CertificateNumber n) = n

instance Type CertificateNumber where
  parseExpr str
    | matchTest regExp str = Right . CertificateNumber $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[0-9]{20}"
    where
      regExp = mkRegex "^[0-9]{20}$"

  render (CertificateNumber n) = unpack n
