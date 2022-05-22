module CFDI.Types.Concept where

import CFDI.Chainable
import CFDI.Types.Amount
import CFDI.Types.ConceptTaxes
import CFDI.Types.CustomInfo
import CFDI.Types.MeasurementUnit
import CFDI.Types.ProductDescription
import CFDI.Types.ProductId
import CFDI.Types.ProductOrService
import CFDI.Types.ProductUnit
import CFDI.Types.PropertyAccount
import CFDI.Types.Quantity
import CFDI.Types.TaxObject
import CFDI.XmlNode
import Data.Maybe                    (catMaybes, maybeToList)

data Concept = Concept
  { conAmount     :: Amount
  , conCustomInfo :: [CustomInfo]
  , conDesc       :: ProductDescription
  , conDiscount   :: Maybe Amount
  , conMeasUnit   :: MeasurementUnit
  , conProdId     :: Maybe ProductId
  , conProdServ   :: ProductOrService
  , conPropAcc    :: Maybe PropertyAccount
  , conQuantity   :: Quantity
  , conTaxes      :: Maybe ConceptTaxes
  , conTaxObject  :: Maybe TaxObject
  , conUnit       :: Maybe ProductUnit
  , conUnitPrice  :: Amount
  } deriving (Eq, Show)

instance Chainable Concept where
  chain c = conProdServ
        <@> conProdId
        <~> conQuantity
        <~> conMeasUnit
        <~> conUnit
        <~> conDesc
        <~> conUnitPrice
        <~> conAmount
        <~> conDiscount
        <~> conTaxes
        <~> conCustomInfo
       <~~> conPropAcc
        <~> (c, "")

instance XmlNode Concept where
  attributes n =
    [ attr "Importe"       $ conAmount n
    , attr "Descripcion"   $ conDesc n
    , attr "ClaveUnidad"   $ conMeasUnit n
    , attr "ClaveProdServ" $ conProdServ n
    , attr "Cantidad"      $ conQuantity n
    , attr "ValorUnitario" $ conUnitPrice n
    ] ++ catMaybes
    [ attr "Descuento"        <$> conDiscount n
    , attr "NoIdentificacion" <$> conProdId n
    , attr "Unidad"           <$> conUnit n
    , attr "ObjetoImp"        <$> conTaxObject n
    ]

  children n = maybeToList (renderNode <$> conTaxes n)
            ++ map renderNode (conCustomInfo n)
            ++ maybeToList (renderNode <$> conPropAcc n)

  nodeName = const "Concepto"

  parseNode n = Concept
    <$> requireAttribute "Importe" n
    <*> parseChildren "InformacionAduanera" n
    <*> requireAttribute "Descripcion" n
    <*> parseAttribute "Descuento" n
    <*> requireAttribute "ClaveUnidad" n
    <*> parseAttribute "NoIdentificacion" n
    <*> requireAttribute "ClaveProdServ" n
    <*> parseChild "CuentaPredial" n
    <*> requireAttribute "Cantidad" n
    <*> parseChild "Impuestos" n
    <*> parseAttribute "ObjetoImp" n
    <*> parseAttribute "Unidad" n
    <*> requireAttribute "ValorUnitario" n
