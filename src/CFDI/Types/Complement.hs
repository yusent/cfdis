module CFDI.Types.Complement where

import CFDI.Types.PacStamp
import CFDI.XmlNode

data Complement = Complement
  { pacStamp :: Maybe PacStamp
  } deriving (Eq, Show)

instance XmlNode Complement where
  parseNode n = Complement <$> parseChild "TimbreFiscalDigital" n
