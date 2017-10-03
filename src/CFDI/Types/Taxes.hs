module CFDI.Types.Taxes where

import CFDI.Types.Amount
import CFDI.Types.RetainedTaxes
import CFDI.Types.TransferedTaxes
import CFDI.XmlNode

data Taxes = Taxes
  { retainedTaxes   :: Maybe RetainedTaxes
  , totalTransfered :: Maybe Amount
  , totalRetained   :: Maybe Amount
  , transferedTaxes :: Maybe TransferedTaxes
  } deriving (Eq, Show)

instance XmlNode Taxes where
  parseNode n = Taxes
    <$> parseChild "Retenciones" n
    <*> parseAttribute "TotalImpuestosTrasladados" n
    <*> parseAttribute "TotalImpuestosRetenidos" n
    <*> parseChild "Traslados" n
