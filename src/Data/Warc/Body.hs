module Data.Warc.Body where

import Codec.Compression.GZip                (compress, decompress)
import Data.Attoparsec.ByteString.Lazy as L  (Parser, take)
import Data.ByteString                       (ByteString)
import Data.ByteString.Builder               (byteString)
import Data.ByteString.Lazy                  (fromStrict, toStrict)

import Data.Warc.Common                      (ToBuilder (..))
import Data.Warc.Value                       (CompressionMode (..))

data WarcBody = CompressedBody      {-# UNPACK #-} !ByteString
              | UncompressedBody    {-# UNPACK #-} !ByteString

data BodyDetails = BodyDetails {-# UNPACK #-} !Int  !CompressionMode

warcbody :: Int -> CompressionMode -> Parser WarcBody
warcbody sz Compressed = CompressedBody <$> L.take sz
warcbody sz Uncompressed = UncompressedBody <$> L.take sz

compressBody :: WarcBody -> WarcBody
compressBody cb@(CompressedBody _)    = cb
compressBody    (UncompressedBody bs) = CompressedBody (compress' bs)
    where
    compress' = toStrict . compress . fromStrict 

decompressBody :: WarcBody -> WarcBody
decompressBody ub@(UncompressedBody _) = ub
decompressBody    (CompressedBody bs)  = UncompressedBody (decompress' bs)
    where
    decompress' = toStrict . decompress . fromStrict 

instance ToBuilder WarcBody where
    toBuilder (CompressedBody bs) = byteString bs
    toBuilder (UncompressedBody bs) = byteString bs