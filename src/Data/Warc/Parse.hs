module Data.Warc.Parse where

import Data.Warc.WarcEntry

import Data.Attoparsec.ByteString.Lazy     (Result(..), parse)
import Data.ByteString.Lazy            as L

fromByteString :: L.ByteString -> [Either String WarcEntry]
fromByteString bs
    | L.null bs = []
    | otherwise = case parse warcEntry bs of
                      (Done remainder we) -> Right we : fromByteString remainder
                      x -> [Left "error"]
