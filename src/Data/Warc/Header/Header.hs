{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.Header.Header where

import Data.Attoparsec.ByteString.Lazy  (Parser, many1, string)
import Data.ByteString.Builder          (Builder, toLazyByteString, byteString, intDec, char8)
import Data.ByteString.Char8     as C8  (ByteString, concat)
import Data.ByteString.Lazy             (toStrict)
import Data.Char                        (isSpace)

import Data.Warc.Header.HeaderLine hiding (build)
import qualified Data.Warc.Header.HeaderLine as HL (build, getContentLength, getCompressionMode)
import Data.Warc.Shared

data WarcHeader = WarcHeader WarcVersion [HeaderLine] deriving (Eq, Show)

newtype WarcVersion = WarcVersion ByteString deriving (Eq, Show)

getContentLength :: WarcHeader -> Maybe Int
getContentLength (WarcHeader _ headers) = HL.getContentLength headers

getCompressionMode :: WarcHeader -> Maybe CompressionMode
getCompressionMode (WarcHeader _ headers) = HL.getCompressionMode headers

warcHeader :: Parser WarcHeader
warcHeader = WarcHeader <$> version
                        <*  crlf
                        <*> many1 headerLine

version :: Parser WarcVersion
version = WarcVersion <$> (string "WARC/" *> takeTill1 isSpace)

toByteString :: WarcHeader -> ByteString
toByteString = toStrict . toLazyByteString . build

build :: WarcHeader -> Builder
build (WarcHeader (WarcVersion ver) headerLines) =
    mconcat $ [byteString "WARC/", byteString ver, byteString "\r\n"] ++ map HL.build headerLines
