{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.WarcEntry where

import Data.Attoparsec.ByteString.Lazy (Parser)

import Data.ByteString.Char8    as C8

import Data.Warc.Body.Body  as Body
import Data.Warc.Header.Header  as Header
import Data.Warc.Shared

data WarcEntry = WarcEntry WarcHeader WarcBody deriving (Eq, Show)

warcEntry :: Parser WarcEntry
warcEntry = do

    header <- warcHeader

    crlf

    body <- let (Just contentLength) = getContentLength header
                (Just compressionMode) = getCompressionMode header
            in warcbody contentLength compressionMode

    crlf
    crlf

    pure $ WarcEntry header body

toByteString :: WarcEntry -> ByteString
toByteString (WarcEntry header body) =
    C8.concat [
        Header.toByteString header,
        "\r\n",
        Body.toByteString body,
        "\r\n",
        "\r\n"]
