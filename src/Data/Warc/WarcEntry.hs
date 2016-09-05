{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.WarcEntry where

import Data.Attoparsec.ByteString.Lazy      (Parser)
import Data.ByteString.Builder              (Builder, byteString)
import qualified Data.ByteString.Builder as B (toLazyByteString)
import Data.ByteString                      (ByteString)
import Data.ByteString.Lazy                 (toStrict)
import qualified Data.ByteString.Lazy as Lazy (ByteString)

import Data.Warc.Body.Body hiding (build)
import qualified Data.Warc.Body.Body as B (build)
import Data.Warc.Header.Header hiding (build)
import qualified Data.Warc.Header.Header as H
import Data.Warc.Shared
import Data.Warc.Header.Key hiding (build)
import Data.Warc.Header.Value hiding (build)

data WarcEntry = WarcEntry !WarcHeader !WarcBody

warcEntry :: Parser WarcEntry
warcEntry = do

    header <- warcHeader

    crlf

    body <- do
        contentLength <- getContentLength header
        compressionMode <- getCompressionMode header
        warcbody contentLength compressionMode

    crlf
    crlf

    pure $ WarcEntry header body

    where
    getContentLength :: WarcHeader -> Parser Int
    getContentLength header = case getValue (MandatoryKey ContentLength) header of
        Just (IntValue i) -> return i
        Nothing -> fail "Could not find content-length"

    getCompressionMode :: WarcHeader -> Parser CompressionMode
    getCompressionMode header = case getValue (CustomKey CompressionMode) header of
        Just (CompressionModeValue c) -> return c
        Nothing -> return Uncompressed

toByteString :: WarcEntry -> ByteString
toByteString = toStrict . toLazyByteString

toLazyByteString :: WarcEntry -> Lazy.ByteString
toLazyByteString = B.toLazyByteString . build

build :: WarcEntry -> Builder
build (WarcEntry header body) =
    mconcat [H.build header, byteString "\r\n", B.build body, byteString "\r\n\r\n"]
