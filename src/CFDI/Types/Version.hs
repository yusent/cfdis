module CFDI.Types.Version where

import CFDI.Types.Type

data Version = Version Float deriving (Eq, Show)

instance Type Version where
  parseExpr "3.3" = Right $ Version 3.3
  parseExpr e     = Left  $ InvalidValue e

  render _ = "3.3"
