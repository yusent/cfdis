module CFDI.Types.Concepts where

import CFDI.Chainable
import CFDI.Types.Concept
import CFDI.XmlNode
import Data.Text          (intercalate)

newtype Concepts = Concepts [Concept] deriving (Eq, Show)

instance Chainable Concepts where
  chain (Concepts cs) = intercalate "|" $ chain <$> cs

instance XmlNode Concepts where
  children (Concepts cs) = renderNode <$> cs

  nodeName = const "Conceptos"

  parseNode n = do
    concs <- parseChildren "Concepto" n
    case concs of
      [] -> Left  $ ExpectedAtLeastOne "Concepto"
      _  -> Right $ Concepts concs
