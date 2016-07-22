module Data.Warc.WarcEntry where

import Data.Attoparsec.ByteString.Lazy (Parser)

import Data.Warc.Body.Body
import Data.Warc.Header.Header
import Data.Warc.Shared

data WarcEntry = WarcEntry WarcHeader WarcBody deriving Show

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
