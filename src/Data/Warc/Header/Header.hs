{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.Header.Header where

import Data.Attoparsec.ByteString.Lazy  (Parser, many1, string)
import Data.ByteString                  (ByteString)
import Data.Char                        (isSpace)

import Data.Warc.Header.HeaderLine as HL
import Data.Warc.Shared

data WarcHeader = WarcHeader WarcVersion [HeaderLine] deriving Show

newtype WarcVersion = WarcVersion ByteString deriving Show

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
