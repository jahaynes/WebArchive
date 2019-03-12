module Data.Warc.Parse where

import Data.Warc.WarcEntry

import Data.Attoparsec.ByteString.Lazy      (Result(..), parse)
import Data.ByteString.Lazy            as L

fromByteString :: L.ByteString -> [Either String WarcEntry]
fromByteString bs
    | L.null bs = []
    | otherwise = case parse warcEntry bs of
                      (Done remainder we) -> Right we : fromByteString remainder
                      _ -> [Left "error"]

fromByteStringRemainder :: L.ByteString -> Either String (WarcEntry, L.ByteString)
fromByteStringRemainder bs
    | L.null bs = Left "no data"
    | otherwise = case parse warcEntry bs of
                      (Done remainder we) -> Right (we, remainder)
                      _                   -> Left "error"
