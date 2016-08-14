{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.WarcEntry where

import Data.Attoparsec.ByteString.Lazy (Parser)

import Data.ByteString.Char8    as C8

import Data.Warc.Body.Body  as Body
import Data.Warc.Header.Header  as Header
import Data.Warc.Shared
import Data.Warc.Header.Key
import Data.Warc.Header.Value

data WarcEntry = WarcEntry WarcHeader WarcBody deriving (Eq, Show)

warcEntry :: Parser WarcEntry
warcEntry = do

    header <- warcHeader

    crlf

    contentLength <- getContentLength header
    compressionMode <- getCompressionMode header
    body <- warcbody contentLength compressionMode

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
toByteString (WarcEntry header body) =
    C8.concat [
        Header.toByteString header,
        "\r\n",
        Body.toByteString body,
        "\r\n",
        "\r\n"]
