module CFDI.Types.SatLegend where

import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype SatLegend = SatLegend Text deriving (Eq, Show)

instance Type SatLegend where
  parseExpr str
    | matchTest regExp str = Right . SatLegend $ pack str
    | otherwise = Left
                $ DoesNotMatchExpr "([A-Z]|[a-z]|[0-9]| |Ñ|ñ|!|\"|%|&|'|´|-|:|;\
                                   \|>|=|<|@|_|,|\\{|\\}|`|~|á|é|í|ó|ú|Á|É|Í|Ó|\
                                   \Ú|ü|Ü){12,150}"
    where
      regExp = mkRegex "^([A-Z]|[a-z]|[0-9]| |Ñ|ñ|!|\"|%|&|'|´|-|:|;|>|=|<|@|_|\
                       \,|\\{|\\}|`|~|á|é|í|ó|ú|Á|É|Í|Ó|Ú|ü|Ü){12,150}$"

  render (SatLegend l) = unpack l
