{-# LANGUAGE FlexibleInstances #-}

module CFDI.Chain (originalChain) where

import CFDI
import Data.Time.Calendar  (Day, showGregorian)
import Data.Time.Format    (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime)

class Chainable a where
  chain :: a -> String

  infixr 9 <@>, <@@>, <~>, <~~>

  -- Chain starters
  (<@>) :: Chainable b => (a -> b) -> (a, String) -> String
  (<@>) f = tail . snd . (f <~>)

  (<@@>) :: Chainable b => (a -> [b]) -> (a, String) -> String
  (<@@>) f = tail . snd . (f <~~>)

  -- Chain connectors
  (<~>) :: Chainable b => (a -> b) -> (a, String) -> (a, String)
  f <~> (x, s) = (x, s ++ s')
    where
      s'  = if length s'' > 0 then '|' : s'' else ""
      s'' = chain $ f x

  (<~~>) :: Chainable b => (a -> [b]) -> (a, String) -> (a, String)
  f <~~> (x, s) = (x, s ++ s')
    where
      s' = concat . map (('|' :) . chain) $ f x

instance Chainable Address where
  chain x = zipCode
        <@> country
        <~> state
        <~> municipality
        <~> reference
        <~> locality
        <~> suburb
        <~> internalNumber
        <~> externalNumber
        <~> street
        <~> (x, "")

instance Chainable CFDI where
  chain x = taxes
        <@> concepts
       <~~> recipient
        <~> issuer
        <~> originalAmount
        <~> originalIssuedAt
        <~> originalSeries
        <~> originalNumber
        <~> accountNumber
        <~> issuedIn
        <~> paymentMethod
        <~> total
        <~> currency
        <~> exchangeRate
        <~> discount
        <~> subTotal
        <~> paymentConditions
        <~> wayToPay
        <~> _type
        <~> issuedAt
        <~> version
        <~> (x, "|")

instance Chainable Concept where
  chain x = propertyAccount
        <@> importInfo
       <~~> amount
        <~> unitAmount
        <~> description
        <~> _id
        <~> unit
        <~> quantity
        <~> (x, "")

instance Chainable Day where
  chain = showGregorian

instance Chainable FiscalAddress where
  chain x = fiscalZipCode
        <@> fiscalCountry
        <~> fiscalState
        <~> fiscalMunicipality
        <~> fiscalReference
        <~> fiscalLocality
        <~> fiscalSuburb
        <~> fiscalInternalNumber
        <~> fiscalExternalNumber
        <~> fiscalStreet
        <~> (x, "")

instance Chainable Float where
  chain = show

instance Chainable ImportInfo where
  chain x = custom
        <@> importIssuedAt
        <~> importNumber
        <~> (x, "")

instance Chainable Issuer where
  chain x = regimes
       <@@> issuedInAddress
        <~> fiscalAddress
        <~> name
        <~> rfc
        <~> (x, "")

instance Chainable LocalTime where
  chain = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

instance Chainable a => Chainable (Maybe a) where
  chain = maybe "" chain

instance Chainable PropertyAccount where
  chain = propertyAccountNumber

instance Chainable Recipient where
  chain x = recipientAddress
        <@> recipientName
        <~> recipientRfc
        <~> (x, "")

instance Chainable RetainedTax where
  chain x = retainedTaxAmount
        <@> retainedTax
        <~> (x, "")

instance Chainable String where
  chain = id

instance Chainable Tax where
  chain = show

instance Chainable Taxes where
  chain x = totalTransfered
        <@> transferedTaxes
       <~~> totalRetained
        <~> retainedTaxes
       <~~> (x, "")

instance Chainable TaxRegime where
  chain = regime

instance Chainable TransferedTax where
  chain x = transferedTaxAmount
        <@> transferedTaxRate
        <~> transferedTax
        <~> (x, "")

originalChain :: CFDI -> String
originalChain = ('|' :) . (++ "||") . chain
