module CFDI.Types.Payments where

import CFDI.Chainable
import CFDI.Types.Payment
import CFDI.Types.PaymentsVersion
import CFDI.XmlNode
import Data.Text                  (Text, append, intercalate)

data Payments = Payments
  { pmsPayments :: [Payment]
  , pmsVersion  :: PaymentsVersion
  } deriving (Eq, Show)

instance Chainable Payments where
  chain c = append (chain $ pmsVersion c)
          . append "|"
          . intercalate "|"
          $ chain <$> pmsPayments c

instance XmlNode Payments where
  attributes n =
    [ attrWithPrefix "xsi" "schemaLocation"
        ("http://www.sat.gob.mx/Pagos \
         \http://www.sat.gob.mx/sitio_internet/cfd/Pagos/Pagos10.xsd" :: Text)
    , attrWithPrefix "xmlns" "pago10" ("http://www.sat.gob.mx/Pagos" :: Text)
    , attr "Version" $ pmsVersion n
    ]

  children (Payments ps _) = renderNode <$> ps

  nodeName = const "Pagos"

  nodePrefix = const "pago10"

  parseNode n = do
    pms <- parseChildren "Pago" n
    case pms of
      [] -> Left  $ ExpectedAtLeastOne "Pago"
      _  -> Payments pms <$> requireAttribute "Version" n
