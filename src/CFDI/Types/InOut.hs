module CFDI.Types.InOut where

import CFDI.Chainable
import CFDI.Types.Type

data InOut = In | Out deriving (Eq, Show)

instance Chainable InOut where
  chain In = "Entrada"
  chain Out = "Salida"

instance Type InOut where
  parseExpr "Entrada" = Right In
  parseExpr "Salida" = Right Out
  parseExpr e    = Left  $ InvalidValue e

  render In = "Entrada"
  render Out = "Salida"
