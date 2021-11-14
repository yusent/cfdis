module CFDI.Types.PositiveInt where

import CFDI.Chainable
import CFDI.Types.Type
import CFDI.XmlNode

newtype PositiveInt = PositiveInt Int deriving (Eq, Show)

instance Chainable PositiveInt where
  chain (PositiveInt i) = chain i

instance Type PositiveInt where
  parseExpr e = case filter (>0) (fst <$> reads e) of
    (i : _) -> Right $ PositiveInt i
    _ -> Left $ InvalidValue e

  render (PositiveInt i) = render i
