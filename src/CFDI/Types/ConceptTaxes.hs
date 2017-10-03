module CFDI.Types.ConceptTaxes where

import CFDI.Types.ConceptRetainedTaxes
import CFDI.Types.ConceptTransferedTaxes
import CFDI.XmlNode

data ConceptTaxes = ConceptTaxes
  { conceptRetainedTaxes   :: Maybe ConceptRetainedTaxes
  , conceptTransferedTaxes :: Maybe ConceptTransferedTaxes
  } deriving (Eq, Show)

instance XmlNode ConceptTaxes where
  parseNode n = ConceptTaxes
    <$> parseChild "Retenciones" n
    <*> parseChild "Traslados" n