{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.Header.Key where

import Data.Attoparsec.ByteString.Char8       (Parser, choice)
import Data.Char                              (isSpace)
import Data.ByteString.Char8            as C8 (ByteString)

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
toByteString (MandatoryKey WarcRecordId) = "WARC-Record-ID"
toByteString (MandatoryKey ContentLength) = "Content-Length"
toByteString (MandatoryKey WarcDate) = "WARC-Date"
toByteString (MandatoryKey WarcType) = "WARC-Type"
toByteString (OptionalKey ContentType) = "Content-Type"
toByteString (OptionalKey WarcTargetURI) = "WARC-Target-URI"
toByteString (CustomKey CompressionMode) = "Compression-Mode"
toByteString (CustomKey UncompressedContentLength) = "Uncompressed-Content-Length"
toByteString (CustomKey (UnknownKey unknownKey)) = unknownKey
