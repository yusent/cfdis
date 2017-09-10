module CFDI.ParserSpec where

import Data.Maybe          (fromJust, isJust, isNothing)
import Data.Time.Calendar  (Day(ModifiedJulianDay))
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Test.Hspec
import CFDI.Parser
import CFDI.Types

invoiceCFDI :: CFDI
invoiceCFDI =
  fromJust $ parseCFDI invoiceXML

invoiceXML :: String
invoiceXML =
  " <?xml version='1.0' encoding='UTF-8'?>                                     \
  \ <cfdi:Comprobante total='536.216' NumCtaPago='1212'                        \
  \   subTotal='1161.07' tipoDeComprobante='ingreso' Moneda='MXN'              \
  \   sello='SOMENOTSORANDOMSIGNATURE'                                         \
  \   certificado='SOMENOTSORANDOMCERTIFICATE' serie='A'                       \
  \   formaDePago='PAGO EN UNA SOLA EXHIBICION' version='3.2'                  \
  \   fecha='2017-06-12T12:00:00' condicionesDePago='CREDITO 15 DIAS'          \
  \   metodoDePago='03' noCertificado='00001000001212121212'                   \
  \   LugarExpedicion='Issued In' folio='144144'>                                 \
  \   <cfdi:Emisor rfc='XAXX010101000' nombre='Issuer Name'>                   \
  \     <cfdi:DomicilioFiscal colonia='Fiscal Suburb'                          \
  \       calle='Fiscal Street' noExterior='Fiscal External Number'            \
  \       municipio='Fiscal Municipality' estado='Fiscal State'                \
  \       pais='Fiscal Country' codigoPostal='Fiscal Zip Code'/>               \
  \     <cfdi:RegimenFiscal Regimen='Fiscal Regime'/>                          \
  \   </cfdi:Emisor>                                                           \
  \   <cfdi:Receptor rfc='XEXX010101000' nombre='Receipient Name'>             \
  \     <cfdi:Domicilio calle='Recipient Street'                               \
  \       municipio='Recipient Municipality' estado='Recipient State'          \
  \       pais='Recipient Country' codigoPostal='Recipient Zip Code'/>         \
  \   </cfdi:Receptor>                                                         \
  \   <cfdi:Conceptos>                                                         \
  \     <cfdi:Concepto noIdentificacion='Product 1 ID' importe='112.00'        \
  \       valorUnitario='112.00' unidad='Product 1 Unit' cantidad='1.00'       \
  \       descripcion='Product 1 Description'>                                 \
  \       <cfdi:InformacionAduanera numero='Product 1 Import Number'           \
  \         fecha='1986-08-22' aduana='Product 1 Custom'/>                     \
  \     </cfdi:Concepto>                                                       \
  \     <cfdi:Concepto noIdentificacion='Product 2 ID' importe='424.24'        \
  \       valorUnitario='212.12' unidad='Product 2 Unit' cantidad='2.00'       \
  \       descripcion='Product 2 Description'>                                 \
  \     </cfdi:Concepto>                                                       \
  \   </cfdi:Conceptos>                                                        \
  \   <cfdi:Impuestos totalImpuestosRetenidos='12.144'                         \
  \     totalImpuestosTrasladados='12.12'>                                     \
  \     <cfdi:Traslados>                                                       \
  \       <cfdi:Traslado importe='12.12' tasa='16' impuesto='IVA'/>            \
  \     </cfdi:Traslados>                                                      \
  \     <cfdi:Retenciones>                                                     \
  \       <cfdi:Retencion importe='12.144' impuesto='ISR'/>                    \
  \     </cfdi:Retenciones>                                                    \
  \   </cfdi:Impuestos>                                                        \
  \   <cfdi:Complemento>                                                       \
  \     <tfd:TimbreFiscalDigital UUID='12121212-1212-1212-1212-121212121212'   \
  \       FechaTimbrado='2015-11-23T23:05:00' version='1.0'                    \
  \       selloCFD='Signature' noCertificadoSAT='SAT Certificate Number'       \
  \       selloSAT='SAT Signature'/>                                           \
  \   </cfdi:Complemento>                                                      \
  \ </cfdi:Comprobante>                                                        "

spec :: Spec
spec = do
  describe "CFDI.Parser.parseCFDI" $ do
    context "when parsing an invoice" $ do
      it "parses account number" $ do
        accountNumber invoiceCFDI `shouldBe` Just "1212"

      it "parses total" $ do
        total invoiceCFDI `shouldBe` 536.216

      it "parses certificate" $ do
        certificate invoiceCFDI `shouldBe` "SOMENOTSORANDOMCERTIFICATE"

      it "parses certificate number" $ do
        certificateNumber invoiceCFDI `shouldBe` "00001000001212121212"

      it "parses complement" $ do
        let _complement = complement invoiceCFDI
        _complement `shouldSatisfy` isJust

        let maybePacStamp = pacStamp $ fromJust _complement
        maybePacStamp `shouldSatisfy` isJust

        let _pacStamp = fromJust maybePacStamp
        cfdSignature _pacStamp `shouldBe` "Signature"
        satCertificateNumber _pacStamp `shouldBe` "SAT Certificate Number"
        satSignature _pacStamp `shouldBe` "SAT Signature"
        localDay (stampedAt _pacStamp) `shouldBe` ModifiedJulianDay 57349
        localTimeOfDay (stampedAt _pacStamp) `shouldBe` TimeOfDay 23 5 0
        stampVersion _pacStamp `shouldBe` "1.0"
        uuid _pacStamp `shouldBe` "12121212-1212-1212-1212-121212121212"

      it "parses concepts" $ do
        let _concepts = concepts invoiceCFDI
        length _concepts `shouldBe` 2

        let (concept0 : concept1 : _) = _concepts
        amount concept0 `shouldBe` 112
        description concept0 `shouldBe` "Product 1 Description"
        _id concept0 `shouldBe` Just "Product 1 ID"
        let _importInfo = importInfo concept0
        length _importInfo `shouldBe` 1
        let __importInfo = head _importInfo
        custom __importInfo `shouldBe` Just "Product 1 Custom"
        importIssuedAt __importInfo `shouldBe` ModifiedJulianDay 46664
        importNumber __importInfo `shouldBe` "Product 1 Import Number"
        quantity concept0 `shouldBe` 1
        unit concept0 `shouldBe` "Product 1 Unit"
        unitAmount concept0 `shouldBe` 112

        amount concept1 `shouldBe` 424.24
        description concept1 `shouldBe` "Product 2 Description"
        _id concept1 `shouldBe` Just "Product 2 ID"
        let _importInfo = importInfo concept1
        length _importInfo `shouldBe` 0
        quantity concept1 `shouldBe` 2
        unit concept1 `shouldBe` "Product 2 Unit"
        unitAmount concept1 `shouldBe` 212.12

      it "parses currency" $ do
        currency invoiceCFDI `shouldBe` Just "MXN"

      it "parses internal ID" $ do
        internalID invoiceCFDI `shouldBe` Just "144144"

      it "parses issued date" $ do
        localDay (issuedAt invoiceCFDI) `shouldBe` ModifiedJulianDay 57916
        localTimeOfDay (issuedAt invoiceCFDI) `shouldBe` TimeOfDay 12 0 0

      it "parses where it was issued in" $ do
        issuedIn invoiceCFDI `shouldBe` "Issued In"

      it "parses issuer" $ do
        let _issuer = issuer invoiceCFDI
        let maybeFiscalAddress = fiscalAddress _issuer
        maybeFiscalAddress `shouldSatisfy` isJust
        let _fiscalAddress = fromJust maybeFiscalAddress
        fiscalCountry _fiscalAddress `shouldBe` "Fiscal Country"
        fiscalExternalNumber _fiscalAddress `shouldBe` Just "Fiscal External Number"
        fiscalInternalNumber _fiscalAddress `shouldBe` Nothing
        fiscalLocality _fiscalAddress `shouldBe` Nothing
        fiscalMunicipality _fiscalAddress `shouldBe` "Fiscal Municipality"
        fiscalReference _fiscalAddress `shouldBe` Nothing
        fiscalState _fiscalAddress `shouldBe` "Fiscal State"
        fiscalStreet _fiscalAddress `shouldBe` "Fiscal Street"
        fiscalSuburb _fiscalAddress `shouldBe` Just "Fiscal Suburb"
        fiscalZipCode _fiscalAddress `shouldBe` "Fiscal Zip Code"
        issuedInAddress _issuer `shouldSatisfy` isNothing
        name _issuer `shouldBe` Just "Issuer Name"
        regimes _issuer `shouldBe` [TaxRegime "Fiscal Regime"]
        rfc _issuer `shouldBe` "XAXX010101000"
