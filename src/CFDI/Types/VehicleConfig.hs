module CFDI.Types.VehicleConfig where

import CFDI.Types.Type
import Data.Set (fromList, member)
import Data.Text (Text, pack, unpack)

newtype VehicleConfig = VehicleConfig Text deriving (Eq, Show)

instance Type VehicleConfig where
  parseExpr c
    | c `member` validCodes = Right . VehicleConfig $ pack c
    | otherwise = Left NotInCatalog
    where
      validCodes = fromList
        [ "C2", "C3", "C2R2", "C3R2", "C2R3", "C3R3", "T2S1", "T2S2", "T2S3"
        , "T3S1", "T3S2", "T3S3", "T2S1R2", "T2S2R2", "T2S1R3", "T3S1R2"
        , "T3S1R3", "T3S2R2", "T3S2R3", "T3S2R4", "T2S2S2", "T3S2S2", "T3S3S2"
        , "OTROEV", "OTROEGP", "OTROSG", "VL", "OTROEVGP", "GPLUTA", "GPLUTB"
        , "GPLUTC", "GPLUTD", "GPLATA", "GPLATB", "GPLATC", "GPLATD"
        ]

  render (VehicleConfig x) = unpack x
