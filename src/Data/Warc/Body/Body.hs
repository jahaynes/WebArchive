module Data.Warc.Body.Body where

import Data.Attoparsec.ByteString.Lazy as L  (Parser, take)
import Data.ByteString                       (ByteString)

import Data.Warc.Shared

data WarcBody = CompressedBody ByteString
              | UncompressedBody ByteString deriving Show

data BodyDetails = BodyDetails Int CompressionMode

warcbody :: Int -> CompressionMode -> Parser WarcBody
warcbody sz Compressed = CompressedBody <$> L.take sz
warcbody sz Uncompressed = UncompressedBody <$> L.take sz