{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.Header.Header where

import Data.Attoparsec.ByteString.Lazy  (Parser, many1, string)
import Data.ByteString.Builder          (Builder, toLazyByteString, byteString)
import Data.ByteString                  (ByteString)
import Data.ByteString.Lazy             (toStrict)
import Data.Char                        (isSpace)

import Data.Warc.Header.Key             (Key)
import Data.Warc.Header.Value           (Value)
import Data.Warc.Header.HeaderLine      (HeaderLine, headerLine)
import qualified Data.Warc.Header.HeaderLine as HL (build, getValue)
import Data.Warc.Shared

data WarcHeader = WarcHeader {-# UNPACK #-} !WarcVersion [HeaderLine]

newtype WarcVersion = WarcVersion ByteString

getValue :: Key -> WarcHeader -> Maybe Value
getValue key (WarcHeader _ headers) = HL.getValue key headers

warcHeader :: Parser WarcHeader
warcHeader = WarcHeader <$> version
                        <*  crlf
                        <*> many1 headerLine

version :: Parser WarcVersion
version = WarcVersion <$> (string "WARC/" *> takeTill1 isSpace)

build :: WarcHeader -> Builder
build (WarcHeader (WarcVersion ver) headerLines) =
    mconcat $ [byteString "WARC/", byteString ver, byteString "\r\n"] ++ map HL.build headerLines
