module CFDI.Types.Payment where

import CFDI.Chainable
import CFDI.Types.AccountNumber
import CFDI.Types.Amount
import CFDI.Types.BankName
import CFDI.Types.BankRfc
import CFDI.Types.CompanyRfc
import CFDI.Types.Currency
import CFDI.Types.ExchangeRate
import CFDI.Types.OperationId
import CFDI.Types.PaymentChain
import CFDI.Types.PaymentChainType
import CFDI.Types.PaymentRelatedDocument
import CFDI.Types.PaymentTaxes
import CFDI.Types.WayToPay
import CFDI.XmlNode
import Data.Maybe                        (catMaybes)
import Data.Text                         (Text)
import Data.Time.LocalTime               (LocalTime)

data Payment = Payment
  { pmAmount       :: Amount
  , pmBankRfc      :: Maybe BankRfc
  , pmCertificate  :: Maybe Text
  , pmChain        :: Maybe PaymentChain
  , pmChainType    :: Maybe PaymentChainType
  , pmCurrency     :: Currency
  , pmExchangeRate :: Maybe ExchangeRate
  , pmIssAccount   :: Maybe AccountNumber
  , pmIssBankName  :: Maybe BankName
  , pmIssuedAt     :: LocalTime
  , pmOperationId  :: Maybe OperationId
  , pmRecAccount   :: Maybe AccountNumber
  , pmRecBankRfc   :: Maybe CompanyRfc
  , pmRelatedDocs  :: [PaymentRelatedDocument]
  , pmSignature    :: Maybe Text
  , pmTaxes        :: [PaymentTaxes]
  , pmWayToPay     :: WayToPay
  } deriving (Eq, Show)

instance Chainable Payment where
  chain c = pmIssuedAt
        <@> pmWayToPay
        <~> pmCurrency
        <~> pmExchangeRate
        <~> pmAmount
        <~> pmOperationId
        <~> pmBankRfc
        <~> pmIssBankName
        <~> pmIssAccount
        <~> pmRecBankRfc
        <~> pmRecAccount
        <~> pmChainType
        <~> pmCertificate
        <~> pmChain
        <~> pmSignature
        <~> pmRelatedDocs
       <~~> (c, "")

instance XmlNode Payment where
  attributes n =
    [ attr "FechaPago"    $ pmIssuedAt n
    , attr "FormaDePagoP" $ pmWayToPay n
    , attr "MonedaP"      $ pmCurrency n
    , attr "Monto"        $ pmAmount n
    ] ++ catMaybes
    [ attr "TipoCambioP" <$> pmExchangeRate n
    , attr "NumOperacion" <$> pmOperationId n
    , attr "RfcEmisorCtaOrd" <$> pmBankRfc n
    , attr "NomBancoOrdExt" <$> pmIssBankName n
    , attr "CtaOrdenante" <$> pmIssAccount n
    , attr "RfcEmisorCtaBen" <$> pmRecBankRfc n
    , attr "CtaBeneficiario" <$> pmRecAccount n
    , attr "TipoCadPago" <$> pmChainType n
    , attr "CertPago" <$> pmCertificate n
    , attr "CadPago" <$> pmChain n
    , attr "SelloPago" <$> pmSignature n
    ]

  children n =
    (renderNode <$> pmRelatedDocs n) ++ (renderNode <$> pmTaxes n)

  nodeName = const "Pago"

  nodePrefix = const "pago10"

  parseNode n = Payment
    <$> requireAttribute "Monto" n
    <*> parseAttribute "RfcEmisorCtaOrd" n
    <*> parseAttribute "CertPago" n
    <*> parseAttribute "CadPago" n
    <*> parseAttribute "TipoCadPago" n
    <*> requireAttribute "MonedaP" n
    <*> parseAttribute "TipoCambioP" n
    <*> parseAttribute "CtaOrdenante" n
    <*> parseAttribute "NomBancoOrdExt" n
    <*> requireAttribute "FechaPago" n
    <*> parseAttribute "NumOperacion" n
    <*> parseAttribute "CtaBeneficiario" n
    <*> parseAttribute "RfcEmisorCtaBen" n
    <*> parseChildren "DoctoRelacionado" n
    <*> parseAttribute "SelloPago" n
    <*> parseChildren "Impuestos" n
    <*> requireAttribute "FormaDePagoP" n
