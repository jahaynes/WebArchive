{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.Header.HeaderLine where

import Data.Attoparsec.ByteString.Char8      (Parser, skipSpace, char)

import Data.Warc.Header.Key     as Key
import Data.Warc.Header.Value   as Value
import Data.Warc.Shared

import Data.ByteString.Char8    as C8

data HeaderLine = HeaderLine Key Value deriving (Eq, Show)

headerLine :: Parser HeaderLine
headerLine = do
    k <- key <* valueSeparator
    v <- value k <* crlf
    pure $ HeaderLine k v

valueSeparator :: Parser ()
valueSeparator = skipSpace >> char ':' >> skipSpace

getValue :: Key -> [HeaderLine] -> Maybe Value
getValue   _ [] = Nothing
getValue key (HeaderLine k v:xs) | k == key = Just v
                                 | otherwise = getValue key xs 

toByteString :: HeaderLine -> ByteString
toByteString (HeaderLine k v) = C8.concat [Key.toByteString k, ": ", Value.toByteString v, "\r\n"]