module CFDI.Types.RelatedCfdis where

import CFDI.Types.RelatedCfdi
import CFDI.Types.RelationshipType
import CFDI.XmlNode

data RelatedCfdis = RelatedCfdis
  { related          :: [RelatedCfdi]
  , relationshipType :: RelationshipType
  } deriving (Eq, Show)

instance XmlNode RelatedCfdis where
  parseNode n = do
    relCfdis <- parseChildren "CfdiRelacionado" n
    case relCfdis of
      [] -> Left $ ExpectedAtLeastOne "CfdiRelacionado"
      _  -> RelatedCfdis relCfdis <$> requireAttribute "TipoRelacion" n
