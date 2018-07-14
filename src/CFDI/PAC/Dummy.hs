{-# LANGUAGE OverloadedStrings #-}

module CFDI.PAC.Dummy
  ( Dummy(..)
  ) where

import CFDI
import CFDI.PAC
import Data.Text (append)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)

data Dummy = Dummy

instance PAC Dummy where
  cancelCFDI _ (UUID uuid) = return . Right
    $ "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">\n\
      \  <s:Body xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\">\n\
      \    <CancelaCFDResponse xmlns=\"http://cancelacfd.sat.gob.mx\">\n\
      \      <CancelaCFDResult Fecha=\"1986-08-22T23:05:59.3141592\" RfcEmisor=\"TEST010101000\">\n\
      \        <Folios>\n\
      \          <UUID>" `append` uuid `append` "</UUID>\n\
      \          <EstatusUUID>201</EstatusUUID>\n\
      \        </Folios>\n\
      \        <Signature Id=\"SelloSAT\" xmlns=\"http://www.w3.org/2000/09/xmldsig#\">\n\
      \          <SignedInfo>\n\
      \            <CanonicalizationMethod Algorithm=\"http://www.w3.org/TR/2001/REC-xml-c14n-20010315\"/>\n\
      \            <SignatureMethod Algorithm=\"http://www.w3.org/2001/04/xmldsig-more#hmac-sha512\"/>\n\
      \            <Reference URI=\"\">\n\
      \              <Transforms>\n\
      \                <Transform Algorithm=\"http://www.w3.org/TR/1999/REC-xpath-19991116\">\n\
      \                  <XPath>not(ancestor-or-self::*[local-name()='Signature'])</XPath>\n\
      \                </Transform>\n\
      \              </Transforms>\n\
      \              <DigestMethod Algorithm=\"http://www.w3.org/2001/04/xmlenc#sha512\"/>\n\
      \              <DigestValue>dummydigestdummydigestdummydigestdummydigestdummydigestdummydigestdummydigestdummydigest</DigestValue>\n\
      \            </Reference>\n\
      \          </SignedInfo>\n\
      \          <SignatureValue>dummysignaturedummysignaturedummysignaturedummysignaturedummysignaturedummysignaturedumm</SignatureValue>\n\
      \          <KeyInfo>\n\
      \            <KeyName>12121212121212121212</KeyName>\n\
      \            <KeyValue>\n\
      \              <RSAKeyValue>\n\
      \                <Modulus>dummymodulusdummymodulusdummymodulusdummymodulusdummymodulusdummymodulusdummymodulusdummymodulusdummymodulusdummymodulusdummymodulusdummymodulusdummymodulusdummymodulusdumm</Modulus>\n\
      \                <Exponent>AQAB</Exponent>\n\
      \              </RSAKeyValue>\n\
      \            </KeyValue>\n\
      \          </KeyInfo>\n\
      \        </Signature>\n\
      \      </CancelaCFDResult>\n\
      \    </CancelaCFDResponse>\n\
      \  </s:Body>\n\
      \</s:Envelope>"

  getPacStamp _  = stampLookup

  stampLookup _ _ = do
    currentTime <- utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

    return . Right $ PacStamp
      { psLegend = Just $ SatLegend "No olvide pagar sus impuestos para mantener el Status Quo"
      , psPacRfc = CompanyRfc "XXX010107357"
      , psSatCerNum = CertificateNumber "00001007357473577357"
      , psSatSig = "PnMHIiszK38aZQTHXI4veuddGQMEGyMq5aaVuqXSv0kS/pc1DWHN+Wwm8KQgk/xRE03qMs1NrEAR3JDy42He1P8gX+cD1GT6qPXPHxleLCzT0s/DrUoEr8+losxKsz8FuPJRA5/OWJTcOoRHi/CC2Z/GmIxUqSdDAmKnvMyocNtWZ8ShA+eK+QJIpw4H/H72PXNWBYmdPnsStfzTbVW6MCWTUoeRxLNofHDMNHpnK3sE6EzI0lPPKYqy1zDe8A5RwjayzqxkCYSzO7gteBz1XlKGrCoKXwqyl/dMlUMelY122vFZydE1RV/z3uqL4OWfj6FbQCJIdxNK+2xKXFng3Q=="
      , psSignature = "ApXJB9/HUF0imr757WvXkfMxPYI9jDZb81vAghZWCJnxGISp9AuwODhiWteGlQh7VwQmCRFSSFhOvq+t0yu2QmeFvMYqMUm8p1kHD5AlJh0A3WYs7tOqtQSfivY/nVCs9jU0kt4GqMjibB9gjLPT0gTEpUokf43FODpDPemqWSL/DDgetCurrentTimeZoneTBBDIdbWvaqj689RE4q5D9b9YDeCk2CE5OQONLvw6d9ta8TzIs0p0ITjfeRGHVeog1tOYUfi5ICwSTF9t2NU5NWdSXP1H3+cCAhNw7hphKncrrpBhoHZdtq5CovObz9qvnvEj9dken8EjCP+gdRqd5xcbdRuVFwBHIipXAhw=="
      , psStampedAt = currentTime
      , psUuid = UUID "73577357-7357-7357-7357-735773577357"
      , psVersion = PacStampVersion 1.1
      }
