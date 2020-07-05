module CFDI.Types.CustomInfo where

import CFDI.Chainable
import CFDI.Types.ImportNumber
import CFDI.XmlNode

newtype CustomInfo = CustomInfo ImportNumber deriving (Eq, Show)

instance Chainable CustomInfo where
  chain (CustomInfo i) = chain i

instance XmlNode CustomInfo where
  attributes (CustomInfo n) = [attr "NumeroPedimento" n]

  nodeName = const "InformacionAduanera"

  parseNode n = CustomInfo <$> requireAttribute "NumeroPedimento" n
