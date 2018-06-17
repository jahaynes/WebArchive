{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Warc.Key where

import Data.Attoparsec.ByteString.Char8       (Parser, choice, isSpace)
import Data.ByteString.Builder                (byteString)
import Data.ByteString                        (ByteString)
import GHC.Generics                           (Generic)

import Data.Warc.Common                       (ToBuilder (..))
import Data.Warc.Shared

data Key = MandatoryKey !MandatoryKey 
         | OptionalKey  !OptionalKey
         | CustomKey    !CustomKey deriving (Eq, Generic)

data MandatoryKey = WarcRecordId
                  | ContentLength
                  | WarcDate
                  | WarcType deriving (Eq, Generic)

data OptionalKey = ContentType
                 | WarcTargetURI deriving (Eq, Generic)

data CustomKey = CompressionMode
               | UncompressedContentLength 
               | UnknownKey {-# UNPACK #-} !ByteString deriving (Eq, Generic)

key :: Parser Key
key = choice [ MandatoryKey <$> choice [ WarcRecordId <%> "WARC-Record-ID"
                                       , ContentLength <%> "Content-Length"
                                       , WarcDate <%> "WARC-Date"
                                       , WarcType <%> "WARC-Type" ]

             , OptionalKey <$> choice  [ ContentType <%> "Content-Type"
                                       , WarcTargetURI <%> "WARC-Target-URI" ]

             , CustomKey <$> choice    [ CompressionMode <%> "Compression-Mode"
                                       , UnknownKey <$> takeTill1 (\x -> isSpace x || x == ':') ]]

instance ToBuilder Key where
    toBuilder (MandatoryKey k) = toBuilder k
    toBuilder (OptionalKey k)  = toBuilder k
    toBuilder (CustomKey k)    = toBuilder k

instance ToBuilder MandatoryKey where
    toBuilder WarcRecordId  = "WARC-Record-ID"
    toBuilder ContentLength = "Content-Length"
    toBuilder WarcDate      = "WARC-Date"
    toBuilder WarcType      = "WARC-Type"

instance ToBuilder OptionalKey where
    toBuilder ContentType   = byteString "Content-Type"
    toBuilder WarcTargetURI = byteString "WARC-Target-URI"    

instance ToBuilder CustomKey where
    toBuilder           CompressionMode = byteString "Compression-Mode"
    toBuilder UncompressedContentLength = byteString "Uncompressed-Content-Length"
    toBuilder   (UnknownKey unknownKey) = byteString unknownKey
