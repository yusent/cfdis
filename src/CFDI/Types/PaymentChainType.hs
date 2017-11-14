module CFDI.Types.PaymentChainType where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text       (Text)

newtype PaymentChainType = PaymentChainType Text deriving (Eq, Show)

instance Chainable PaymentChainType where
  chain (PaymentChainType v) = chain v

instance Type PaymentChainType where
  parseExpr "01" = Right $ PaymentChainType "01"
  parseExpr _    = Left NotInCatalog

  render _ = "01"
