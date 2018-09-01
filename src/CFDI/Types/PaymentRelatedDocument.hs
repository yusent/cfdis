module CFDI.Types.PaymentRelatedDocument where

import CFDI.Chainable
import CFDI.Types.Amount
import CFDI.Types.Currency
import CFDI.Types.Folio
import CFDI.Types.ExchangeRate
import CFDI.Types.Partiality
import CFDI.Types.PaymentMethod
import CFDI.Types.RelatedDocumentId
import CFDI.Types.Series
import CFDI.XmlNode
import Data.Maybe                   (catMaybes)

data PaymentRelatedDocument = PaymentRelatedDocument
  { prdCurrency :: Currency
  , prdExRate   :: Maybe ExchangeRate
  , prdId       :: RelatedDocumentId
  , prdFolio    :: Maybe Folio
  , prdPaid     :: Maybe Amount
  , prdPart     :: Maybe Partiality
  , prdPayMet   :: PaymentMethod
  , prdPrevBal  :: Maybe Amount
  , prdRemain   :: Maybe Amount
  , prdSeries   :: Maybe Series
  } deriving (Eq, Show)

instance Chainable PaymentRelatedDocument where
  chain c = prdId
        <@> prdSeries
        <~> prdFolio
        <~> prdCurrency
        <~> prdExRate
        <~> prdPayMet
        <~> prdPart
        <~> prdPrevBal
        <~> prdPaid
        <~> prdRemain
        <~> (c, "")

instance XmlNode PaymentRelatedDocument where
  attributes n =
    [ attr "MonedaDR" $ prdCurrency n
    , attr "IdDocumento" $ prdId n
    , attr "MetodoDePagoDR" $ prdPayMet n
    ] ++ catMaybes
    [ attr "Folio" <$> prdFolio n
    , attr "Serie" <$> prdSeries n
    , attr "TipoCambioDR" <$> prdExRate n
    , attr "NumParcialidad" <$> prdPart n
    , attr "ImpSaldoAnt" <$> prdPrevBal n
    , attr "ImpPagado" <$> prdPaid n
    , attr "ImpSaldoInsoluto" <$> prdRemain n
    ]

  nodeName = const "DoctoRelacionado"

  nodePrefix = const "pago10"

  parseNode n = PaymentRelatedDocument
    <$> requireAttribute "MonedaDR" n
    <*> parseAttribute "TipoCambioDR" n
    <*> requireAttribute "IdDocumento" n
    <*> parseAttribute "Folio" n
    <*> parseAttribute "ImpPagado" n
    <*> parseAttribute "NumParcialidad" n
    <*> requireAttribute "MetodoDePagoDR" n
    <*> parseAttribute "ImpSaldoAnt" n
    <*> parseAttribute "ImpSaldoInsoluto" n
    <*> parseAttribute "Serie" n
