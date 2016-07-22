module Data.Warc.Header.HeaderLine where

import Data.Attoparsec.ByteString.Char8      (Parser, skipSpace, char)

import Data.Warc.Header.Key
import Data.Warc.Header.Value
import Data.Warc.Shared

data HeaderLine = HeaderLine Key Value deriving Show

headerLine :: Parser HeaderLine
headerLine = do
    k <- key <* valueSeparator
    v <- value k <* crlf
    pure $ HeaderLine k v

valueSeparator :: Parser ()
valueSeparator = skipSpace >> char ':' >> skipSpace

getContentLength :: [HeaderLine] -> Maybe Int
getContentLength [] = Nothing
getContentLength (HeaderLine (MandatoryKey ContentLength) (IntValue i):_) = Just i
getContentLength (_:hs) = getContentLength hs

getCompressionMode :: [HeaderLine] -> Maybe CompressionMode
getCompressionMode [] = Nothing
getCompressionMode (HeaderLine (CustomKey CompressionMode) (CompressionModeValue c):_) = Just c
getCompressionMode (_:hs) = getCompressionMode hs
