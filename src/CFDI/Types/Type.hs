{-# LANGUAGE FlexibleInstances #-}

module CFDI.Types.Type where

import Control.Error.Safe  (justErr)
import Data.Bifunctor      (first)
import Data.Ratio          (denominator, numerator)
import Data.Text           (Text, pack, unpack)
import Data.Text.Read      (rational)
import Data.Time.LocalTime (LocalTime)
import Data.Time.Format    (defaultTimeLocale, formatTime, parseTimeM)
import Numeric             (fromRat, showFFloat)

data ParseError
  = InvalidValue String
  | DoesNotMatchExpr String
  | NotInCatalog
  deriving (Eq, Show)

class Type t where
  parse :: String -> Either ParseError t
  parse = parseExpr . sanitize

  parseExpr :: String -> Either ParseError t

  render :: t -> String

instance Type LocalTime where
  parseExpr = justErr (DoesNotMatchExpr expr)
            . parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
    where
      expr = "(20[1-9][0-9])-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])T(([01]\
             \[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9])"

  render = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

instance Type Int where
  parseExpr e = case reads e of
    ((i, _) : _) -> Right i
    _ -> Left (InvalidValue e)

  render = show

instance Type Rational where
  parseExpr e = first (const $ InvalidValue e) . fmap fst . rational $ pack e

  render r
    | denominator r == 1 = show $ numerator r
    | otherwise = showFFloat Nothing (fromRat r :: Double) ""

instance Type Text where
  parseExpr = Right . pack

  render = unpack

sanitize :: String -> String
sanitize = collapse . removePipes
  where
    collapse = unwords . words
    removePipes = filter (/= '|')
