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
  f <~> (x, s) = (x, s' ++ s)
    where
      s'  = if length s'' > 0 then '|' : s'' else ""
      s'' = chain $ f x

  (<~~>) :: Chainable b => (a -> [b]) -> (a, String) -> (a, String)
  f <~~> (x, s) = (x, s' ++ s)
    where
      s' = concat . map (('|' :) . chain) $ f x

instance Chainable Address where
  chain x = street
        <@> externalNumber
        <~> internalNumber
        <~> suburb
        <~> locality
        <~> reference
        <~> municipality
        <~> state
        <~> country
        <~> zipCode
        <~> (x, "")

instance Chainable CFDI where
  chain x = version
        <@> issuedAt
        <~> _type
        <~> wayToPay
        <~> paymentConditions
        <~> subTotal
        <~> discount
        <~> exchangeRate
        <~> currency
        <~> total
        <~> paymentMethod
        <~> issuedIn
        <~> accountNumber
        <~> originalNumber
        <~> originalSeries
        <~> originalIssuedAt
        <~> originalAmount
        <~> issuer
        <~> recipient
        <~> concepts
       <~~> taxes
        <~> (x, "")

instance Chainable Concept where
  chain x = quantity
        <@> unit
        <~> _id
        <~> description
        <~> unitAmount
        <~> amount
        <~> importInfo
       <~~> propertyAccount
        <~> (x, "")

instance Chainable Day where
  chain = showGregorian

instance Chainable FiscalAddress where
  chain x = fiscalStreet
        <@> fiscalExternalNumber
        <~> fiscalInternalNumber
        <~> fiscalSuburb
        <~> fiscalLocality
        <~> fiscalReference
        <~> fiscalMunicipality
        <~> fiscalState
        <~> fiscalCountry
        <~> fiscalZipCode
        <~> (x, "")

instance Chainable Float where
  chain = show

instance Chainable ImportInfo where
  chain x = importNumber
        <@> importIssuedAt
        <~> custom
        <~> (x, "")

instance Chainable Issuer where
  chain x = rfc
        <@> name
        <~> fiscalAddress
        <~> issuedInAddress
        <~> regimes
       <~~> (x, "")

instance Chainable LocalTime where
  chain = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

instance Chainable a => Chainable (Maybe a) where
  chain = maybe "" chain

instance Chainable PropertyAccount where
  chain = propertyAccountNumber

instance Chainable Recipient where
  chain x = recipientRfc
        <@> recipientName
        <~> recipientAddress
        <~> (x, "")

instance Chainable RetainedTax where
  chain x = retainedTax
        <@> retainedTaxAmount
        <~> (x, "")

instance Chainable String where
  chain = id

instance Chainable Tax where
  chain = show

instance Chainable Taxes where
  chain x = retainedTaxes
       <@@> totalRetained
        <~> transferedTaxes
       <~~> totalTransfered
        <~> (x, "")

instance Chainable TaxRegime where
  chain = regime

instance Chainable TransferedTax where
  chain x = transferedTax
        <@> transferedTaxRate
        <~> transferedTaxAmount
        <~> (x, "")

originalChain :: CFDI -> String
originalChain cfdi = "||" ++ chain cfdi ++ "||"
