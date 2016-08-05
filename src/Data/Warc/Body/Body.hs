module Data.Warc.Body.Body where

import Data.Attoparsec.ByteString.Lazy as L  (Parser, take)
import Data.ByteString                       (ByteString)
import Data.ByteString.Builder               (Builder, toLazyByteString, byteString)
import Data.ByteString.Lazy                  (toStrict)

import Data.Warc.Shared

data WarcBody = CompressedBody ByteString
              | UncompressedBody ByteString deriving (Eq, Show)

data BodyDetails = BodyDetails Int CompressionMode

warcbody :: Int -> CompressionMode -> Parser WarcBody
warcbody sz Compressed = CompressedBody <$> L.take sz
warcbody sz Uncompressed = UncompressedBody <$> L.take sz

toByteString :: WarcBody -> ByteString
toByteString = toStrict . toLazyByteString . build

build :: WarcBody -> Builder
build (CompressedBody bs) = byteString bs
build (UncompressedBody bs) = byteString bs
