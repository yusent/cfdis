{-# OPTIONS_GHC -fno-warn-orphans #-}

module CFDI.V3_2.Chain
  ( originalChain
  ) where

import CFDI.Chainable
import CFDI.V3_2.Types
import Data.Text       (Text, append, pack)

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

instance Chainable Tax where
  chain = pack . show

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

originalChain :: CFDI -> Text
originalChain cfdi = "||" `append` chain cfdi `append` "||"
