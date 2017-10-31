module CFDI.Types.Complement where

import CFDI.Types.PacStamp
import CFDI.XmlNode

data Complement
  = StampComplement
      { pacStamp :: PacStamp
      }
  | PaymentComplement
  deriving (Eq, Show)

instance XmlNode Complement where
  children n = [renderNode $ pacStamp n]

  nodeName = const "Complemento"

  parseNode n = StampComplement <$> requireChild "TimbreFiscalDigital" n
