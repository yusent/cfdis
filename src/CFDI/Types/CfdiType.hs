module CFDI.Types.CfdiType where

import CFDI.Types.Type

data CfdiType
  = Income
  | Outcome
  | Transfer
  | Paysheet
  | Payment
  deriving (Eq, Show)

instance Type CfdiType where
  parseExpr "I" = Right Income
  parseExpr "E" = Right Outcome
  parseExpr "T" = Right Transfer
  parseExpr "N" = Right Paysheet
  parseExpr "P" = Right Payment
  parseExpr _   = Left NotInCatalog

  render Income   = "I"
  render Outcome  = "E"
  render Transfer = "T"
  render Paysheet = "N"
  render Payment  = "P"
