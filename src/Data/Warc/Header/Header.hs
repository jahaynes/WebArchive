{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.Header.Header where

import Data.Attoparsec.ByteString.Lazy  (Parser, many1, string)
import Data.ByteString.Builder          (Builder, toLazyByteString, byteString, intDec, char8)
import Data.ByteString.Char8     as C8  (ByteString, concat)
import Data.ByteString.Lazy             (toStrict)
import Data.Char                        (isSpace)

<<<<<<< 39d0c59a8ceb5ec1ab1c29162ba4de76362c7baf
import Data.Warc.Header.Key             (Key)
import Data.Warc.Header.Value           (Value)
import Data.Warc.Header.HeaderLine as HeaderLine
=======
import Data.Warc.Header.HeaderLine hiding (build)
import qualified Data.Warc.Header.HeaderLine as HL (build, getContentLength, getCompressionMode)
>>>>>>> added a bunch of bytestring builders
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
toByteString = toStrict . toLazyByteString . build

build :: WarcHeader -> Builder
build (WarcHeader (WarcVersion ver) headerLines) =
    mconcat $ [byteString "WARC/", byteString ver, byteString "\r\n"] ++ map HL.build headerLines
