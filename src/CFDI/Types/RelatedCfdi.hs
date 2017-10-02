module CFDI.Types.RelatedCfdi where

import CFDI.Types.UUID
import CFDI.XmlNode

data RelatedCfdi = RelatedCfdi UUID deriving (Eq, Show)

instance XmlNode RelatedCfdi where
  parseNode n = RelatedCfdi <$> requireAttribute "UUID" n
