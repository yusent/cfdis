module CFDI.Types.WaybillComplementVersion where

import CFDI.Chainable
import CFDI.Types.Type

newtype WaybillComplementVersion = WaybillComplementVersion Float deriving (Eq, Show)

instance Chainable WaybillComplementVersion where
  chain (WaybillComplementVersion v) = chain v

instance Type WaybillComplementVersion where
  parseExpr "1.0" = Right $ WaybillComplementVersion 1.0
  parseExpr e     = Left  $ InvalidValue e

  render _ = "1.0"
