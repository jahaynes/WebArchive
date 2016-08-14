{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.Header.Header where

import Data.Attoparsec.ByteString.Lazy  (Parser, many1, string)
import Data.ByteString.Char8     as C8  (ByteString, concat)
import Data.Char                        (isSpace)

import Data.Warc.Header.Key             (Key)
import Data.Warc.Header.Value           (Value)
import Data.Warc.Header.HeaderLine as HeaderLine
import Data.Warc.Shared

data WarcHeader = WarcHeader WarcVersion [HeaderLine] deriving (Eq, Show)

newtype WarcVersion = WarcVersion ByteString deriving (Eq, Show)

getValue :: Key -> WarcHeader -> Maybe Value
getValue key (WarcHeader _ headers) = HeaderLine.getValue key headers

warcHeader :: Parser WarcHeader
warcHeader = WarcHeader <$> version
                        <*  crlf
                        <*> many1 headerLine

version :: Parser WarcVersion
version = WarcVersion <$> (string "WARC/" *> takeTill1 isSpace)

toByteString :: WarcHeader -> ByteString
toByteString (WarcHeader (WarcVersion ver) headerLines) =
    let first = C8.concat ["WARC/", ver, "\r\n"]
        rest = C8.concat $ map HeaderLine.toByteString headerLines
    in C8.concat [first, rest]