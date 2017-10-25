module CFDISpec (spec) where

import CFDI
import Data.Either         (isRight)
import Data.Maybe          (fromJust, isJust)
import Data.Time.Calendar  (Day(ModifiedJulianDay))
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Extra               (replace)
import System.Directory    (removeFile)
import System.IO.Temp      (writeSystemTempFile)
import Test.Hspec
import Text.XML.Light      (Element(..), QName(..), onlyElems, parseXMLDoc)

cfdi :: CFDI
cfdi = CFDI
  (Just (CertificateNumber "00001000001212121212"))
  (Just "MIIGejCCBGKgAwIBAgIUMDAwMDEwMDAwMDA0MDM1MDE3NDIwDQYJKoZIhvcNAQELBQAwgg\
        \GyMTgwNgYDVQQDDC9BLkMuIGRlbCBTZXJ2aWNpbyBkZSBBZG1pbmlzdHJhY2nDs24gVHJp\
        \YnV0YXJpYTEvMC0GA1UECgwmU2VydmljaW8gZGUgQWRtaW5pc3RyYWNpw7NuIFRyaWJ1dG\
        \FyaWExODA2BgNVBAsML0FkbWluaXN0cmFjacOzbiBkZSBTZWd1cmlkYWQgZGUgbGEgSW5m\
        \b3JtYWNpw7NuMR8wHQYJKoZIhvcNAQkBFhBhY29kc0BzYXQuZ29iLm14MSYwJAYDVQQJDB\
        \1Bdi4gSGlkYWxnbyA3NywgQ29sLiBHdWVycmVybzEOMAwGA1UEEQwFMDYzMDAxCzAJBgNV\
        \BAYTAk1YMRkwFwYDVQQIDBBEaXN0cml0byBGZWRlcmFsMRQwEgYDVQQHDAtDdWF1aHTDqW\
        \1vYzEVMBMGA1UELRMMU0FUOTcwNzAxTk4zMV0wWwYJKoZIhvcNAQkCDE5SZXNwb25zYWJs\
        \ZTogQWRtaW5pc3RyYWNpw7NuIENlbnRyYWwgZGUgU2VydmljaW9zIFRyaWJ1dGFyaW9zIG\
        \FsIENvbnRyaWJ1eWVudGUwHhcNMTYwODI2MTgzODMwWhcNMjAwODI2MTgzOTEwWjCB6DEk\
        \MCIGA1UEAxMbSk9TRSBKT0FRVUlOIExJTkNPTE4gQ09SUkFMMSQwIgYDVQQpExtKT1NFIE\
        \pPQVFVSU4gTElOQ09MTiBDT1JSQUwxJDAiBgNVBAoTG0pPU0UgSk9BUVVJTiBMSU5DT0xO\
        \IENPUlJBTDELMAkGA1UEBhMCTVgxMjAwBgkqhkiG9w0BCQEWI21hcmNvbWFydGluZXouY2\
        \9uc3VsdG9yZXNAZ21haWwuY29tMRYwFAYDVQQtEw1MSUNKNzkwNDA4SDM4MRswGQYDVQQF\
        \ExJMSUNKNzkwNDA4SEJDTlJRMDUwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQ\
        \C8+x6GljZ0U4olZYclYQXh3cyFuw1p79zo6js/9xrjW7kc3VHJAv8smcnSkikki7P+J3wl\
        \z6NoqGIYXvHurXqszLNucScGyjgB81G2hEDv4o8ybid36C89vckRjExz8aM39uJe1VQJuH\
        \Rlwcuzmp8tvDX37nkTfgFGV5x+Gh+69QsS3gncI+x8utNPbdO/456svs8sTa2h/ZmWT5Kk\
        \W2LmRHxHDsKf5xB2A+Le4MYIAMMUOYLENbGffih4OaFnQlkUJ6YK1vmmhEalYw1IKY8HxY\
        \zbePZx6AisT8JYWsVCc0wGnio4gTLbKFMQKGrDLF3wx4llCmS98TmJHFYqpmixAgMBAAGj\
        \TzBNMAwGA1UdEwEB/wQCMAAwCwYDVR0PBAQDAgPYMBEGCWCGSAGG+EIBAQQEAwIFoDAdBg\
        \NVHSUEFjAUBggrBgEFBQcDBAYIKwYBBQUHAwIwDQYJKoZIhvcNAQELBQADggIBAJbhVgVp\
        \iLaRoZJOfv0hY4SPJSVlD+nhIMN61TYAU8egA8rnAnBkfjG71urkI03B/FvcUoCwMnViDX\
        \z5tyUcSa1a6URSTpV9loHQ/QLWeTE4bWf5L8HkbjYfpyNMYAzcSc+FXcDhcsiljrJohzZm\
        \NBG106MDOJup9BreUa/OD4Az4Yfvj0HoufGGEi7CbP00+L1hp4/DuJ2m8+FjaGlWI0Vkc0\
        \X3CwtjUl+M/BrD3IweeVSThIoATdltdJVj2aBHyGSSi211BVxJTrpCdse30nCIcjiEuuCI\
        \2bBAIuX/RkF8QyZxQvA619IiA4ix9LfR3q87QNXuILLd4lkZG1wdYbrUcGwgx5anBbDb/H\
        \d4To+bmHGAMrKmeEsSly0yTqpE+BvajvMPCXYe18c5uYSjP/0dg4UHD5khmMr/aI6bcX4m\
        \CceaIxoLBWdW9YDHY7FKad1Byvlg5YO3j8PxhOKtHjXkf2tO6XDr5tb4xkmYK8unEqgMWq\
        \efvLVNAI4iNMP8cX1fGpd8SN1Lr4TaoiON0mZps8rcUS2DRP2+l9VdtnBV8BO0DXCFXFo9\
        \4FTlC9sWLcSsoO8+ZQjzKIRe2XyzfoXXj4Ax2ZNoQdItK7GOS10AD/KsoHvlj0O4leN1pG\
        \Pg5SYmI1ppvP5IIQKqqL6rLdW1iCenyhLhWdgMZq82t2Xf")
  Income
  [ Complement
      (Just (PacStamp
              (Just (SatLegend "AVISO PARA EL CONTRIBUYENTE"))
              (CompanyRfc "SAT970701NN3")
              (CertificateNumber "00001000000403258748")
              "VxwwW+2KLIFNppKkfSUMdMTxP/hOzxcLjG1TGSjMrYEZ3goBp18d/LO37jfE7blA\
              \xJH0nnWWYQEH4JPCpyvXSjA1HRooYlsvV81dSDQF9W0ojPpqgKt5E46+dwNNL3yn\
              \wkcQIg0GhLMzZgEhH3Yw/PGHm0dby02Z6gzm6GyFyA50d1fpco/+fQpiBdlxneH+\
              \n+3uO8OnN8V4shM/vFKIbW6HkhjDgOeJLb2mRM7rSAT9jdbFDkfQri/df3laVUs/\
              \gpEoyXEpK7E5C53yfgqWyTASSEPFlgoSSlEVRL7jC7Pb6eY2EynFgQXt6uemzqQV\
              \oCHha+aDHxiaOlrUI+Xu1g=="
              "AXvgW3lSloJK7iSUJsHiBmLAcdCnQZi0yA8XwODvucH6kgdJTTHr256z9XBjIRFG\
              \Q+eUlmSs4USAS3rz2+DMfLS27EkEeReEFF/Mhcf0bM7U91Gm9p7OvAFs9WPbFzhx\
              \P+gvnzr4KdJ9O4CAsfyVNtlTJ+Z+cquDZw5Yun25xfOqObFtv/m7g2+c9dWbBAgf\
              \2vEGUemy1hg9i8nEBCFkx6TIH3SyojLsXwnIoNWQ3CIZzpydfBnVawddcCZcoUiA\
              \NUQBM5rfgYf1CzUoopEIo1l6+UdbyefKNpvYpejcrBUSPY6Q8uQFvf+rm6jtTrsI\
              \c2LNlhiV5fL9mb8MGMWiKw=="
              (LocalTime
                (ModifiedJulianDay 57953)
                (TimeOfDay 14 31 15))
              (UUID "BA87BCD8-411B-41CB-BA9B-81B1A8327099")
              (PacStampVersion 1.1)))
  ]
  (Concepts
    [ Concept
        (Amount 1090.52)
        [CustomInfo (ImportNumber "12  12  1212  1212121")]
        (ProductDescription "COMIDA MEXICANA")
        (Just (Amount 0))
        MU_ACT
        (Just (ProductId "PROD12"))
        (ProductOrService 91111700)
        (Quantity "1")
        (Just (ConceptTaxes
                (Just (ConceptRetainedTaxes
                        [ ConceptRetainedTax
                            (Amount 0)
                            (TaxBase "0.000001")
                            Rate
                            (TaxRate "0.100000")
                            ISR
                        ]))
                (Just (ConceptTransferedTaxes
                        [ ConceptTransferedTax
                            (Amount 174.48)
                            (TaxBase "1090.52")
                            Rate
                            (TaxRate "0.160000")
                            IVA
                        ]))))
        (Just (ProductUnit "N/A"))
        (Amount 1090.52)
    ])
  (Just (Confirmation "AbcD5"))
  CUR_MXN
  (Just (Amount 0))
  (Just (ExchangeRate "12.121212"))
  (Just (Folio "12"))
  (LocalTime
    (ModifiedJulianDay 57953)
    (TimeOfDay 14 27 3))
  (ZipCode 22115)
  (Issuer
    (Just (Name "EMISOR DE PRUEBA"))
    (RFC "XAXX010101000")
    (PeopleWithBusinessActivities))
  (Just (PaymentConditions "CONDICIONES DE PAGO DE PRUEBA"))
  (Just OneTimePayment)
  (Recipient
    (Just GeneralExpenses)
    (Just (Name "RECEPTOR DE PRUEBA"))
    (RFC "XEXX010101000")
    (Just (TaxId "1234567890"))
    (Just CTY_MEX))
  (Just (RelatedCfdis
          [RelatedCfdi (UUID "12121212-1212-1212-1212-121212121212")]
          Substitution))
  (Just (Series "ABC"))
  (Just "AXvgW3lSloJK7iSUJsHiBmLAcdCnQZi0yA8XwODvucH6kgdJTTHr256z9XBjIRFGQ+eUlm\
        \Ss4USAS3rz2+DMfLS27EkEeReEFF/Mhcf0bM7U91Gm9p7OvAFs9WPbFzhxP+gvnzr4KdJ9\
        \O4CAsfyVNtlTJ+Z+cquDZw5Yun25xfOqObFtv/m7g2+c9dWbBAgf2vEGUemy1hg9i8nEBC\
        \Fkx6TIH3SyojLsXwnIoNWQ3CIZzpydfBnVawddcCZcoUiANUQBM5rfgYf1CzUoopEIo1l6\
        \+UdbyefKNpvYpejcrBUSPY6Q8uQFvf+rm6jtTrsIc2LNlhiV5fL9mb8MGMWiKw==")
  (Amount 1090.52)
  (Just (Taxes 
          (Just (RetainedTaxes
                  [ RetainedTax
                      (Amount 0)
                      ISR
                  ]))
          (Just (Amount 174.48))
          (Just (Amount 0))
          (Just (TransferedTaxes
                  [ TransferedTax 
                      (Amount 174.48)
                      Rate
                      (TaxRate "0.160000")
                      IVA
                  ]))))
  (Amount 1265.00)
  (Version 3.3)
  (Just Cash)

spec :: Spec
spec = do
  describe "CFDI.addCsdCerData" $ do
    it "adds CSD certificate data to CFDI" $ do
      let csdCerData = CsdCerData
            { cerExpiresAt = LocalTime (ModifiedJulianDay 0) (TimeOfDay 0 0 0)
            , cerNumber    = "30001000000300023708"
            , cerToText    = "CERTTEXT"
            }
          cfdi' = addCsdCerData csdCerData cfdi
      certNum  cfdi' `shouldBe` Just (CertificateNumber "30001000000300023708")
      certText cfdi' `shouldBe` Just "CERTTEXT"

  describe "CFDI.originalChain" $ do
    it "calculates the CFDI original chain" $ do
      originalChain cfdi `shouldBe`
        "||3.3|ABC|12|2017-07-19T14:27:03|01|00001000001212121212|CONDICIONES D\
        \E PAGO DE PRUEBA|1090.52|0|MXN|12.121212|1265|I|PUE|22115|AbcD5|04|121\
        \21212-1212-1212-1212-121212121212|XAXX010101000|EMISOR DE PRUEBA|612|X\
        \EXX010101000|RECEPTOR DE PRUEBA|MEX|1234567890|G03|91111700|PROD12|1|A\
        \CT|N/A|COMIDA MEXICANA|1090.52|1090.52|0|1090.52|002|Tasa|0.160000|174\
        \.48|0.000001|001|Tasa|0.100000|0|12 12 1212 1212121|001|0|0|002|Tasa|0\
        \.160000|174.48|174.48||"

  describe "CFDI.parseCfdiFile" $ do
    eitherErrOrCfdi <- runIO $ parseCfdiFile "test/xml/invoice_3_3.xml"

    it "parses invoices from an xml file path" $ do
      eitherErrOrCfdi `shouldBe` Right cfdi

  describe "CFDI.parseCfdiXml" $ do
    xmlSource <- runIO $ readFile "test/xml/invoice_3_3.xml"

    it "parses invoices from an xml source" $ do
      parseCfdiXml xmlSource `shouldBe` Right cfdi

    it "returns meaningful errors" $ do
      parseCfdiXml ("" :: String) `shouldBe` Left MalformedXML

      parseCfdiXml (replace "3.3" "_" xmlSource) `shouldBe`
        Left (AttrParseError "Version" $ InvalidValue "_")

      parseCfdiXml (replace "14:27:03" "" xmlSource) `shouldBe`
        Left (AttrParseError "Fecha"
               (DoesNotMatchExpr "(20[1-9][0-9])-(0[1-9]|1[0-2])-(0[1-9]|[12][0\
                                 \-9]|3[01])T(([01][0-9]|2[0-3]):[0-5][0-9]:[0-\
                                 \5][0-9])"))

      parseCfdiXml (replace "MXN" "_" xmlSource) `shouldBe`
        Left (AttrParseError "Moneda" NotInCatalog)

      parseCfdiXml (replace "Emisor" "_" xmlSource) `shouldBe`
        Left (ElemNotFound "Emisor")

      parseCfdiXml (replace "Concepto " "_ " xmlSource) `shouldBe`
        Left (ParseErrorInChild "Conceptos"
               (ExpectedAtLeastOne "Concepto"))

      parseCfdiXml (replace "UUID" "_" xmlSource) `shouldBe`
        Left (ParseErrorInChild "Complemento"
               (ParseErrorInChild "TimbreFiscalDigital"
                 (AttrNotFound "UUID")))

  describe "CFDI.ppXmlParseError" $ do
    it "generates a pretty message from a XmlParseError" $ do
      ppXmlParseError "" (AttrNotFound "ATTR") `shouldBe`
        "No se encontró el atributo \"ATTR\"."

      ppXmlParseError "-" (AttrParseError "ATTR" $ InvalidValue "VAL") `shouldBe`
        "No se pudo interpretar el atributo \"ATTR\":\n\
        \-\"VAL\" no es un valor válido para este atributo."

      ppXmlParseError "-" (AttrParseError "A" $ DoesNotMatchExpr "X") `shouldBe`
        "No se pudo interpretar el atributo \"A\":\n\
        \-No cumple con la expresión \"X\"."

      ppXmlParseError "-" (AttrParseError "ATTR" NotInCatalog) `shouldBe`
        "No se pudo interpretar el atributo \"ATTR\":\n\
        \-No se encuentra en el catálogo de valores permitidos publicado por el\
        \ SAT."

      ppXmlParseError "" (ElemNotFound "ELEM") `shouldBe`
        "No se encontró el elemento \"ELEM\"."

      ppXmlParseError "" (ExpectedAtLeastOne "ELEM") `shouldBe`
        "Se necesita al menos un \"ELEM\"."

      ppXmlParseError "" MalformedXML `shouldBe`
        "XML malformado o inválido."

      ppXmlParseError "-" (ParseErrorInChild "E" $ AttrNotFound "A") `shouldBe`
        "Se encontró un error en el elemento \"E\":\n-\
        \No se encontró el atributo \"A\"."

  describe "CFDI.signWith" $ do
    it "signs a CFDI with CSD supplied" $ do
      pemFilePath <- writeSystemTempFile "csd.pem" testPEM
      eitherErrOrCfdi <- signWith pemFilePath cfdi
      eitherErrOrCfdi `shouldSatisfy` isRight
      let Right cfdi' = eitherErrOrCfdi
      signature cfdi' `shouldBe` Just
        "KqN2Refrv27+X7lsEwIu1QRdOLC4Sjd7OPZUIwt8zg/4qMox9xLQZIk8DL1b5lff7qkqus\
        \qdJ5lCkq4uSHHQ4CJen3BlbeXk+Nn5T2PExjf1X9iWaN1qLrg5Xlrd6lmIbBzifNx5Fjsd\
        \WSz3oL0KAb5EfJ6f7Osc7c2VyDc5sAJr/ZNVG+7onJLMmtD7PrESw/hUF3vA78Aoevnkht\
        \edw3ayPGI6n64SwW8sG3foA/g8lTBxdUzdxryvTR02y7nuB0oCoelAVh8ue5ua9YPpcAQ3\
        \PeFXnUVNyxOwwK3e60ow1oAzjolNMzfZ+F3hM9LfoOPg4E0lC8Q6V6wAmNK99Q=="
      removeFile pemFilePath

  describe "CFDI.toXML" $ do
    it "returns a parsable XML" $ do
      parseCfdiXml (toXML cfdi) `shouldBe` Right cfdi

    it "returns an XML with its elements in the right order" $ do
      let maybeXML = parseXMLDoc $ toXML cfdi
      maybeXML `shouldSatisfy` isJust

      let xml = fromJust maybeXML
      elName xml `shouldBe`
        QName "Comprobante" (Just "http://www.sat.gob.mx/cfd/3") (Just "cfdi")

      let cfdiElems = onlyElems $ elContent xml
          elemNames = map (qName . elName) cfdiElems
      elemNames `shouldBe`
        [ "CfdiRelacionados"
        , "Emisor"
        , "Receptor"
        , "Conceptos"
        , "Impuestos"
        , "Complemento"
        ]

      let conElems = onlyElems . elContent . head . onlyElems . elContent
                   $ cfdiElems !! 3
          conElemNames = map (qName . elName) conElems
      conElemNames `shouldBe` ["Impuestos", "InformacionAduanera"]

      let conTaxElemNames = map (qName . elName) . onlyElems . elContent
                          $ head conElems
      conTaxElemNames `shouldBe` ["Traslados", "Retenciones"]

      let taxElemNames = map (qName . elName) . onlyElems . elContent
                       $ cfdiElems !! 4
      taxElemNames `shouldBe` ["Retenciones", "Traslados"]

testPEM :: String
testPEM =
  "-----BEGIN PRIVATE KEY-----\n\
  \MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQCXVHLByBCIMIr7\n\
  \wGp7Rp2FSDt/u8k3Q9bZCqKWy/rSko0RaNRKz3RicT94MU+TvgAiA0Fzfh0vbjFg\n\
  \2J2lEWpUiBfQjhQnYfzh9j5/lJpDeHLpa9g2pys7VSDvInVzOnPWh5+YTqO4Q8Vz\n\
  \At7jrhldxUuDm5MU1fvTbLzgFdpTHtu0h60gdA4bi7q1G2cF5vv6+Hbc929ri4AX\n\
  \Pg4sveTxvjCLu6DSjPPWgTRnI0ycPvZPlsyYj3d2aUICHnWAQ/vEFsZWAoMYZ7Wt\n\
  \5PcpYFymFwmJNJNKmrrwSdY0kVX+QY4yFGzrLWkKefZ5IB4eOXPYJ8CUVetfDVtH\n\
  \zHg87ikdAgMBAAECggEALS8Z1KJXzVIxLVoWcRh0kAcxPMJlIgsvaz6xrTTaf2Ui\n\
  \mcAjIvMuXPZTbR/MEuD4SS+Pq1xMeoz8UV5cM50vkm3QLoU9n0SyrQVJQ+6q4Npl\n\
  \9SwuMqNXVS/l1YEEcJNTYwq7rE5OtAYIPn7s7i5dhJIUKgeZsu7xcf9VpdLgjVCD\n\
  \qGgJw/EfhagR7iPF+PKoeyRyBZI9xuHmtElHVgn2/Qv/16UJv0YpAqRgVq7YQzZC\n\
  \c7yo0Y2+3dqHabRg+MnIKkN4pBFBzYxsjwM7YUDk/8zFlF5kwCS74ep0JWWSYAJ1\n\
  \3DYDtCYSyWk1DvxX9Srv/S2htZM6MnhboafjLch4QQKBgQDRGGLpYdqGt6/cXKQe\n\
  \JGWFrG33AMiYKrd4NOw7LK7kzrQESeaeAXSwr2eOnNV3tDMyslkjpC05m3Lbefsh\n\
  \Ul6Qj/Qj9PEIpv7e4X4r++O/FsA9X6iQFicMEDzRYYjm4AfFggYrhzmjXh2rNACL\n\
  \KRX5i9wIRGQuoAG7KuZyYWSBuwKBgQC5Rsv75S6FNUpKe8RC2nw13Vaf9uua2W1+\n\
  \spg2pWfKqw88vvFATQOj9A9aFJ+wqrvwRziua5xtbch9gHK7M9Nnl565Tk8muueO\n\
  \OUBaFeHYXsDaYZfTFILOZU4/b6//r6QK2cO892VXyUydbRXavCpRX8s2EoxtwfFG\n\
  \mgbStX+HBwKBgQCICHKJXXU7QhPyrH7FcW5vKgAcu3DFtrzIQr4RvX9HMsdhJucX\n\
  \kuDk9ijMWnJyv1Szvd5KVsxpdx2hdlmQkzMcn9r47alGtMaKIG/ik6zWrCmDhFF4\n\
  \9ECRE5tNqUPU2JmVwILdHMu94kQxFtLntmIqiPgslLoMr2KQ71cfwQcPcwKBgQCk\n\
  \iNKtqCFf+qs26iKonA6iZyV+eXFR2rT6RvAV114NBUxKzebBC6On/h2ECbymz3iH\n\
  \MTiM7NPF+jCKA3/f725WGLfEKF7yLhlknEMhvT0LQVpSlUiXEyf20tBiVXUew4QS\n\
  \fsDtF2bQRtvbEfzOezu5eDCmnGJJNmpmIHLevH+8EQKBgF9Ff09RISQJHbABka8f\n\
  \wj8sdBKWG3TUQ2SwQ9U3L/Y/unuyaRUF+J3wFRYBMQGu0jzLG5TFfAVZAc3VJCBj\n\
  \xG6K8WnJS6OM9ycV0qBa2WnkC7M7uAt4K9IEIqlOljY/R2tBN7qHZwE7nCLS88rv\n\
  \L5YWIiKp71SlXyoGLfM0h7bl\n\
  \-----END PRIVATE KEY-----\n"
