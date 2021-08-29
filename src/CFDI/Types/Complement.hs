module CFDI.Types.Complement where

import CFDI.Types.PacStamp
import CFDI.Types.Payments
import CFDI.Types.WaybillComplement
import CFDI.XmlNode
import Data.Maybe (catMaybes)

data Complement = Complement
  { paymentsComplement :: Maybe Payments
  , waybillComplement :: Maybe WaybillComplement
  , stampComplement :: Maybe PacStamp
  } deriving (Eq, Show)

instance XmlNode Complement where
  children (Complement payments stamp waybill) = catMaybes
    [ renderNode <$> payments
    , renderNode <$> waybill
    , renderNode <$> stamp
    ]

  nodeName = const "Complemento"

  parseNode n = Complement
    <$> parseChild "Pagos" n
    <*> parseChild "CartaPorte" n
    <*> parseChild "TimbreFiscalDigital" n
