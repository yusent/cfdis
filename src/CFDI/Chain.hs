module CFDI.Chain where

import CFDI

class Chainable a where
  addToChain :: a -> String -> String
  addToChain x s =
    s ++ s'
    where
      s' = if length chain' > 0 then "|" ++ chain' else ""
      chain' = chain x

  chain :: a -> String
