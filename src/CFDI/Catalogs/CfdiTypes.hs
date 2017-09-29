{-# LANGUAGE OverloadedStrings #-}

module CFDI.Catalogs.CfdiTypes where

import CFDI.Catalog

data CfdiType
  = Income
  | Outcome
  | Transfer
  | Paysheet
  | Payment
  deriving (Eq, Show)

instance Catalog CfdiType where
  fromCode "I" = Just Income
  fromCode "E" = Just Outcome
  fromCode "T" = Just Transfer
  fromCode "N" = Just Paysheet
  fromCode "P" = Just Payment
  fromCode _   = Nothing

  toCode Income   = "I"
  toCode Outcome  = "E"
  toCode Transfer = "T"
  toCode Paysheet = "N"
  toCode Payment  = "P"
