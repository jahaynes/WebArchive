{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.WarcEntry where

import Data.Attoparsec.ByteString.Lazy (Parser)
import Data.ByteString.Builder         (Builder, toLazyByteString, byteString)
import Data.ByteString                 (ByteString)
import Data.ByteString.Lazy            (toStrict)

import Data.Warc.Body.Body hiding (build)
import qualified Data.Warc.Body.Body as B (build)
import Data.Warc.Header.Header hiding (build)
import qualified Data.Warc.Header.Header as H
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
toByteString = toStrict . toLazyByteString . build

build :: WarcEntry -> Builder
build (WarcEntry header body) =
    mconcat [H.build header, byteString "\r\n", B.build body, byteString "\r\n\r\n"]
        