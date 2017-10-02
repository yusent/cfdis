module CFDI.Types.CFDI where

import CFDI.Types.Amount
import CFDI.Types.CertificateNumber
import CFDI.Types.Currency
import CFDI.Types.ExchangeRate
import CFDI.Types.Folio
import CFDI.Types.PaymentConditions
import CFDI.Types.Series
import CFDI.Types.Version
import CFDI.Types.WayToPay
import Data.Text                    (Text)
import Data.Time.LocalTime          (LocalTime)

data CFDI = CFDI
  { certNumb     :: Maybe CertificateNumber
  , certText     :: Maybe Text
  , currency     :: Currency
  , discount     :: Maybe Amount
  , exchangeRate :: Maybe ExchangeRate
  , folio        :: Maybe Folio
  , issuedAt     :: LocalTime
  , paymentConds :: Maybe PaymentConditions
  , series       :: Maybe Series
  , signature    :: Maybe Text
  , subTotal     :: Amount
  , version      :: Version
  , wayToPay     :: Maybe WayToPay
  }
