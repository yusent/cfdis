module CFDI.Types.PropertyAccount where

import CFDI.Chainable
import CFDI.Types.PropertyAccountNumber
import CFDI.XmlNode

newtype PropertyAccount = PropertyAccount
  { propAccNumber :: PropertyAccountNumber
  } deriving (Eq, Show)

instance Chainable PropertyAccount where
  chain (PropertyAccount a) = chain a

instance XmlNode PropertyAccount where
  attributes (PropertyAccount n) = [attr "Numero" n]

  nodeName = const "CuentaPredial"

  parseNode n = PropertyAccount <$> requireAttribute "Numero" n
