module CFDI.Types.GlobalInvoiceInfo where

import CFDI.Chainable
import CFDI.Types.GlobalInvoiceMonths
import CFDI.Types.GlobalInvoiceYear
import CFDI.Types.Periodicity
import CFDI.XmlNode

data GlobalInvoiceInfo = GlobalInvoiceInfo
  { giPeriodicity :: Periodicity
  , giMonths      :: GlobalInvoiceMonths
  , giYear        :: GlobalInvoiceYear
  } deriving (Eq, Show)

instance Chainable GlobalInvoiceInfo where
  chain c = ""

instance XmlNode GlobalInvoiceInfo where
  attributes n =
    [ attr "Periodicidad" $ giPeriodicity n
    , attr "Meses"        $ giMonths n
    , attr "Año"          $ giYear n
    ]

  nodeName = const "InformacionGlobal"

  parseNode n = GlobalInvoiceInfo
    <$> requireAttribute "Periodicidad" n
    <*> requireAttribute "Meses" n
    <*> requireAttribute "Año" n
