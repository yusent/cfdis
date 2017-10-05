module CFDI.Types.Complement where

import CFDI.Types.PacStamp
import CFDI.XmlNode
import Data.Maybe          (catMaybes)

newtype Complement = Complement
  { pacStamp :: Maybe PacStamp
  } deriving (Eq, Show)

instance XmlNode Complement where
  children n = catMaybes [renderNode <$> pacStamp n]

  nodeName = const "Complemento"

  parseNode n = Complement <$> parseChild "TimbreFiscalDigital" n
