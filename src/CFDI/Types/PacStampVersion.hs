module CFDI.Types.PacStampVersion where

import CFDI.Types.Type

newtype PacStampVersion = PacStampVersion Float deriving (Eq, Show)

instance Type PacStampVersion where
  parseExpr "1.1" = Right $ PacStampVersion 1.1
  parseExpr e     = Left  $ InvalidValue e

  render _ = "1.1"
