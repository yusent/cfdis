{-# LANGUAGE FlexibleInstances #-}

module CFDI.Chain where

import CFDI
import Data.Time.Calendar  (Day, showGregorian)
import Data.Time.Format    (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime)

class Chainable a where
  addToChain :: a -> String -> String
  addToChain x s =
    s ++ s'
    where
      s' = if length chain' > 0 then "|" ++ chain' else ""
      chain' = chain x

  chain :: a -> String

instance Chainable Day where
  chain = showGregorian

instance Chainable Float where
  chain = show

instance Chainable LocalTime where
  chain = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

instance Chainable a => Chainable (Maybe a) where
  chain = maybe "" chain

instance Chainable String where
  chain = id
