{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.Header.Key where

import Data.Attoparsec.ByteString.Char8       (Parser, choice)
import Data.Char                              (isSpace)
import Data.ByteString.Builder                (Builder, toLazyByteString, byteString, intDec, char8)
import Data.ByteString.Char8            as C8 (ByteString)
import Data.ByteString.Lazy.Char8             (toStrict)

import Data.Warc.Shared

data Key = MandatoryKey MandatoryKey 
         | OptionalKey OptionalKey
         | CustomKey CustomKey deriving (Eq, Show)

data MandatoryKey = WarcRecordId
                  | ContentLength
                  | WarcDate
                  | WarcType deriving (Eq, Show)

data OptionalKey = ContentType
                 | WarcTargetURI deriving (Eq, Show)

data CustomKey = CompressionMode
               | UncompressedContentLength 
               | UnknownKey ByteString deriving (Eq, Show)

key :: Parser Key
key = choice [ MandatoryKey <$> choice [ WarcRecordId <%> "WARC-Record-ID"
                                       , ContentLength <%> "Content-Length"
                                       , WarcDate <%> "WARC-Date"
                                       , WarcType <%> "WARC-Type" ]

             , OptionalKey <$> choice  [ ContentType <%> "Content-Type"
                                       , WarcTargetURI <%> "WARC-Target-URI" ]

             , CustomKey <$> choice    [ CompressionMode <%> "Compression-Mode"
                                       , UnknownKey <$> takeTill1 (\x -> isSpace x || x == ':') ]]

toByteString :: Key -> ByteString
toByteString = toStrict . toLazyByteString . build

build :: Key -> Builder
build (MandatoryKey key) = buildMandatory key
build (OptionalKey key)  = buildOptional key
build (CustomKey key)    = buildCustom key

buildMandatory :: MandatoryKey -> Builder
buildMandatory WarcRecordId  = "WARC-Record-ID"
buildMandatory ContentLength = "Content-Length"
buildMandatory WarcDate      = "WARC-Date"
buildMandatory WarcType      = "WARC-Type"

buildOptional :: OptionalKey -> Builder
buildOptional ContentType   = byteString "Content-Type"
buildOptional WarcTargetURI = byteString "WARC-Target-URI"

buildCustom :: CustomKey -> Builder
buildCustom           CompressionMode = byteString "Compression-Mode"
buildCustom UncompressedContentLength = byteString "Uncompressed-Content-Length"
buildCustom   (UnknownKey unknownKey) = byteString unknownKey