{-# LANGUAGE FlexibleInstances #-}

module CFDI.Chain (originalChain) where

import CFDI
import Data.Time.Calendar  (Day, showGregorian)
import Data.Time.Format    (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime)

class Chainable a where
  addToChain :: a -> String -> String
  addToChain x "" = chain x
  addToChain x s  = s ++ s'
    where
      s'     = if length chain' > 0 then "|" ++ chain' else ""
      chain' = chain x

  chain :: a -> String

instance Chainable Address where
  chain x = addToChain (zipCode x)
          . addToChain (country x)
          . addToChain (state x)
          . addToChain (municipality x)
          . addToChain (reference x)
          . addToChain (locality x)
          . addToChain (suburb x)
          . addToChain (internalNumber x)
          . addToChain (externalNumber x)
          $ addToChain (street x) ""

instance Chainable CFDI where
  chain x = addToChain (taxes x)
          . addToChain conceptsChain
          . addToChain (recipient x)
          . addToChain (issuer x)
          . addToChain (originalAmount x)
          . addToChain (originalIssuedAt x)
          . addToChain (originalSeries x)
          . addToChain (originalNumber x)
          . addToChain (accountNumber x)
          . addToChain (issuedIn x)
          . addToChain (paymentMethod x)
          . addToChain (total x)
          . addToChain (currency x)
          . addToChain (exchangeRate x)
          . addToChain (discount x)
          . addToChain (subTotal x)
          . addToChain (paymentConditions x)
          . addToChain (wayToPay x)
          . addToChain (_type x)
          . addToChain (issuedAt x)
          $ addToChain (version x) "|"
    where
      conceptsChain = foldl (flip addToChain) "" $ concepts x

instance Chainable Concept where
  chain x = addToChain (propertyAccount x)
          . addToChain importInfoChain
          . addToChain (amount x)
          . addToChain (unitAmount x)
          . addToChain (description x)
          . addToChain (_id x)
          . addToChain (unit x)
          $ addToChain (quantity x) ""
    where
      importInfoChain = foldl (flip addToChain) "" $ importInfo x

instance Chainable Day where
  chain = showGregorian

instance Chainable FiscalAddress where
  chain x = addToChain (fiscalZipCode x)
          . addToChain (fiscalCountry x)
          . addToChain (fiscalState x)
          . addToChain (fiscalMunicipality x)
          . addToChain (fiscalReference x)
          . addToChain (fiscalLocality x)
          . addToChain (fiscalSuburb x)
          . addToChain (fiscalInternalNumber x)
          . addToChain (fiscalExternalNumber x)
          $ addToChain (fiscalStreet x) ""

instance Chainable Float where
  chain = show

instance Chainable ImportInfo where
  chain x = addToChain (custom x)
          . addToChain (importIssuedAt x)
          $ addToChain (importNumber x) ""

instance Chainable Issuer where
  chain x = addToChain regimesChain
          . addToChain (issuedInAddress x)
          . addToChain (fiscalAddress x)
          . addToChain (name x)
          $ addToChain (rfc x) ""
    where
      regimesChain = foldl (flip addToChain) "" $ regimes x

instance Chainable LocalTime where
  chain = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

instance Chainable a => Chainable (Maybe a) where
  chain = maybe "" chain

instance Chainable PropertyAccount where
  chain = propertyAccountNumber

instance Chainable Recipient where
  chain x = addToChain (recipientAddress x)
          . addToChain (recipientName x)
          $ addToChain (recipientRfc x) ""

instance Chainable RetainedTax where
  chain x = addToChain (retainedTaxAmount x)
          $ addToChain (retainedTax x) ""

instance Chainable String where
  chain = id

instance Chainable Tax where
  chain = show

instance Chainable Taxes where
  chain x = addToChain (totalTransfered x)
          . addToChain transferedTaxesChain
          . addToChain (totalRetained x)
          $ addToChain retainedTaxesChain ""
    where
      retainedTaxesChain = foldl (flip addToChain) "" $ retainedTaxes x
      transferedTaxesChain = foldl (flip addToChain) "" $ transferedTaxes x

instance Chainable TaxRegime where
  chain = regime

instance Chainable TransferedTax where
  chain x = addToChain (transferedTaxAmount x)
          . addToChain (transferedTaxRate x)
          $ addToChain (transferedTax x) ""

originalChain :: CFDI -> String
originalChain = (++ "||") . chain
