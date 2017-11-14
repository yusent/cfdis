module CFDI.Types.Complement where

import CFDI.Types.PacStamp
import CFDI.Types.Payments
import CFDI.XmlNode
import Data.Bifunctor      (first)

data Complement
  = StampComplement
      { pacStamp :: PacStamp
      }
  | PaymentComplement Payments
  deriving (Eq, Show)

instance XmlNode Complement where
  children (StampComplement ps) = [renderNode ps]
  children (PaymentComplement pc) = [renderNode pc]

  nodeName = const "Complemento"

  parseNode n = case stampNode of
    Nothing -> case paymentNode of
      Nothing -> Left UnknownComplement

      Just e -> first (ParseErrorInChild "Pagos")
              $ PaymentComplement <$> parseNode e

    Just e -> first (ParseErrorInChild "TimbreFiscalDigital")
            $ StampComplement <$> parseNode e
    where
      stampNode = findChildByName "TimbreFiscalDigital" n
      paymentNode = findChildByName "Pagos" n
