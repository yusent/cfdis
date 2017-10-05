module CFDI.Types.RelatedCfdi where

import CFDI.Types.UUID
import CFDI.XmlNode

newtype RelatedCfdi = RelatedCfdi UUID deriving (Eq, Show)

instance XmlNode RelatedCfdi where
  attributes (RelatedCfdi uuid) = [attr "UUID" uuid]

  nodeName = const "CfdiRelacionado"

  parseNode n = RelatedCfdi <$> requireAttribute "UUID" n
