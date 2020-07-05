module CFDI.Types.Complement where

import CFDI.Types.PacStamp
import CFDI.Types.Payments
import CFDI.XmlNode
import Data.Maybe          (catMaybes)

data Complement = Complement
  { paymentsComplement :: Maybe Payments
  , stampComplement :: Maybe PacStamp
  } deriving (Eq, Show)

instance XmlNode Complement where
  children (Complement payments stamp) = catMaybes
    [ renderNode <$> payments
    , renderNode <$> stamp
    ]

  nodeName = const "Complemento"

  parseNode n = Complement
    <$> parseChild "Pagos" n
    <*> parseChild "TimbreFiscalDigital" n
