module CFDI.Types.CustomInfo where

import CFDI.Types.ImportNumber
import CFDI.XmlNode

data CustomInfo = CustomInfo ImportNumber deriving (Eq, Show)

instance XmlNode CustomInfo where
  parseNode n = CustomInfo <$> requireAttribute "NumeroPedimento" n
