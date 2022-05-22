module CFDI.Types.Version where

import CFDI.Chainable
import CFDI.Types.Type

newtype Version = Version Float deriving (Eq, Show)

instance Chainable Version where
  chain (Version v) = chain v

instance Type Version where
  parseExpr "4.0" = Right $ Version 4.0
  parseExpr "3.3" = Right $ Version 3.3
  parseExpr e     = Left  $ InvalidValue e

  render (Version 4.0) = "4.0"
  render _ = "3.3"
