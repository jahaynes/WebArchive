{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.Header.Key where

import Data.Attoparsec.ByteString.Char8      (Parser, choice)
import Data.Char                             (isSpace)
import Data.ByteString                       (ByteString)

import Data.Warc.Shared

data Key = MandatoryKey MandatoryKey 
         | OptionalKey OptionalKey
         | CustomKey CustomKey deriving Show

data MandatoryKey = WarcRecordId
                  | ContentLength
                  | WarcDate
                  | WarcType deriving Show

data OptionalKey = ContentType
                 | WarcTargetURI deriving Show

data CustomKey = CompressionMode
               | UncompressedContentLength 
               | UnknownKey ByteString deriving Show

key :: Parser Key
key = choice [ MandatoryKey <$> choice [ WarcRecordId <%> "WARC-Record-ID"
                                       , ContentLength <%> "Content-Length"
                                       , WarcDate <%> "WARC-Date"
                                       , WarcType <%> "WARC-Type" ]

             , OptionalKey <$> choice  [ ContentType <%> "Content-Type"
                                       , WarcTargetURI <%> "WARC-Target-URI" ]

             , CustomKey <$> choice    [ CompressionMode <%> "Compression-Mode"
                                       , UnknownKey <$> takeTill1 (\x -> isSpace x || x == ':') ]]

