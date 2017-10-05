module CFDI.Types.ConceptTaxes where

import CFDI.Chainable
import CFDI.Types.ConceptRetainedTaxes
import CFDI.Types.ConceptTransferedTaxes
import CFDI.XmlNode
import Data.Maybe                        (catMaybes)

data ConceptTaxes = ConceptTaxes
  { conceptRetainedTaxes   :: Maybe ConceptRetainedTaxes
  , conceptTransferedTaxes :: Maybe ConceptTransferedTaxes
  } deriving (Eq, Show)

instance Chainable ConceptTaxes where
  chain c = conceptTransferedTaxes
        <@> conceptRetainedTaxes
        <~> (c, "")

instance XmlNode ConceptTaxes where
  children n = catMaybes
    [ renderNode <$> conceptTransferedTaxes n
    , renderNode <$> conceptRetainedTaxes n
    ]

  nodeName = const "Impuestos"

  parseNode n = ConceptTaxes
    <$> parseChild "Retenciones" n
    <*> parseChild "Traslados" n
