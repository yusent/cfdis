module CFDI.Types.WaybillComplementLocations where

import CFDI.Chainable
import CFDI.XmlNode

data WaybillComplementLocations = WaybillComplementLocations deriving (Eq, Show)

instance Chainable WaybillComplementLocations where
  chain WaybillComplementLocations = ""

instance XmlNode WaybillComplementLocations where
  children WaybillComplementLocations = []

  nodeName = const "Ubicaciones"

  parseNode n = Right WaybillComplementLocations
