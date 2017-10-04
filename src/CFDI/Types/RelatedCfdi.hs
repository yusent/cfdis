module CFDI.Types.RelatedCfdi where

import CFDI.Types.UUID
import CFDI.XmlNode

data RelatedCfdi = RelatedCfdi UUID deriving (Eq, Show)

instance XmlNode RelatedCfdi where
  nodeName = const "CfdiRelacionado"

  parseNode n = RelatedCfdi <$> requireAttribute "UUID" n

  requiredAttributes (RelatedCfdi uuid) = [attr "UUID" uuid]
