module CFDI.Types.CfdiType where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text       (pack)

data CfdiType
  = Income
  | Outcome
  | Transfer
  | Paysheet
  | Payment_
  deriving (Eq, Show)

instance Chainable CfdiType where
  chain = pack . render

instance Type CfdiType where
  parseExpr "I" = Right Income
  parseExpr "E" = Right Outcome
  parseExpr "T" = Right Transfer
  parseExpr "N" = Right Paysheet
  parseExpr "P" = Right Payment_
  parseExpr _   = Left NotInCatalog

  render Income   = "I"
  render Outcome  = "E"
  render Transfer = "T"
  render Paysheet = "N"
  render Payment_ = "P"
