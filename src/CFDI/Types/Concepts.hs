module CFDI.Types.Concepts where

import CFDI.Types.Concept
import CFDI.XmlNode

data Concepts = Concepts [Concept] deriving (Eq, Show)

instance XmlNode Concepts where
  children (Concepts cs) = renderNode <$> cs

  nodeName = const "Conceptos"

  parseNode n = do
    concs <- parseChildren "Concepto" n
    case concs of
      [] -> Left  $ ExpectedAtLeastOne "Concepto"
      _  -> Right $ Concepts concs
