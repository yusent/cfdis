module CFDI.Types.Type where

data ParseError
  = InvalidValue
  | DoesNotMatchExpr Text
  | NotInCatalog
  deriving (Eq, Show)

class Type t where
  parse :: String -> Either ParseError t

  render :: t -> String
