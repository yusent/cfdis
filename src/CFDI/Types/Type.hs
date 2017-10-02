module CFDI.Types.Type where

import Control.Error.Safe  (justErr)
import Data.Text           (Text, pack, unpack)
import Data.Time.LocalTime (LocalTime)
import Data.Time.Format    (defaultTimeLocale, formatTime, parseTimeM)

data ParseError
  = InvalidValue
  | DoesNotMatchExpr Text
  | NotInCatalog
  deriving (Eq, Show)

class Type t where
  parse :: String -> Either ParseError t

  render :: t -> String

instance Type LocalTime where
  parse = justErr (DoesNotMatchExpr expr)
        . parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
    where
      expr = "(20[1-9][0-9])-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])T(([01]\
             \[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9])"

  render = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

instance Type Text where
  parse = Right . pack

  render = unpack
