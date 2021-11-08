module CFDI.Types.Packaging where

import CFDI.Types.Type
import Data.Set (fromList, member)
import Data.Text (Text, pack, unpack)

newtype Packaging = Packaging Text deriving (Eq, Show)

instance Type Packaging where
  parseExpr c
    | c `member` validCodes = Right . Packaging $ pack c
    | otherwise = Left NotInCatalog
    where
      validCodes = fromList
        [ "1A1", "1A2", "1B1", "1B2", "1D", "1G", "1H1", "1H2", "1N1", "1N2"
        , "3A1", "3A2", "3B1", "3B2", "3H1", "3H2", "4A", "4B", "4C1", "4C2"
        , "4D", "4F", "4G", "4H1", "4H2", "5H1", "5H2", "5H3", "5H4", "5L1"
        , "5L2", "5L3", "5M1", "5M2", "6HA1", "6HA2", "6HB1", "6HB2", "6HC"
        , "6HD1", "6HD2", "6HG1", "6HG2", "6HH1", "6HH2", "6PA1", "6PA2"
        , "6PB1", "6PB2", "6PC", "6PD 1", "6PD2", "6PG1", "6PG2", "6PH1"
        , "6PH2", "7H1", "7L1", "Z01"
        ]

  render (Packaging x) = unpack x
