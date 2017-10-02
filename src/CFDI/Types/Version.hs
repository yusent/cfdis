module CFDI.Types.Version where

import CFDI.Types.Type

data Version = Version Float deriving (Eq, Show)

instance Type Version where
  parse "3.3" = Right $ Version 3.3
  parse _     = Left InvalidValue

  render _ = "3.3"
