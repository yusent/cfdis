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
  parse "I" = Right Income
  parse "E" = Right Outcome
  parse "T" = Right Transfer
  parse "N" = Right Paysheet
  parse "P" = Right Payment
  parse _   = Left NotInCatalog

  render Income   = "I"
  render Outcome  = "E"
  render Transfer = "T"
  render Paysheet = "N"
  render Payment  = "P"
