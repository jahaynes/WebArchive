{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.Header.HeaderLine where

import Data.Attoparsec.ByteString.Char8      (Parser, skipSpace, char)
import Data.ByteString.Builder               (Builder, toLazyByteString, byteString)
import Data.ByteString.Char8    as C8
import Data.ByteString.Lazy                  (toStrict)

import Data.Warc.Header.Key       hiding     (build)
import qualified Data.Warc.Header.Key as K   (build)
import Data.Warc.Header.Value     hiding     (build)
import qualified Data.Warc.Header.Value as V (build)
import Data.Warc.Shared

data HeaderLine = HeaderLine Key Value deriving (Eq, Show)

headerLine :: Parser HeaderLine
headerLine = do
    k <- key <* valueSeparator
    v <- value k <* crlf
    pure $ HeaderLine k v

valueSeparator :: Parser ()
valueSeparator = skipSpace >> char ':' >> skipSpace

getContentLength :: [HeaderLine] -> Maybe Int
getContentLength [] = Nothing
getContentLength (HeaderLine (MandatoryKey ContentLength) (IntValue i):_) = Just i
getContentLength (_:hs) = getContentLength hs

getCompressionMode :: [HeaderLine] -> Maybe CompressionMode
getCompressionMode [] = Nothing
getCompressionMode (HeaderLine (CustomKey CompressionMode) (CompressionModeValue c):_) = Just c
getCompressionMode (_:hs) = getCompressionMode hs

toByteString :: HeaderLine -> ByteString
toByteString = toStrict . toLazyByteString . build

build :: HeaderLine -> Builder
build (HeaderLine k v) = mconcat [K.build k, byteString ": ", V.build v, byteString "\r\n"]
