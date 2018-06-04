{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.HeaderLine where

import Data.Attoparsec.ByteString.Char8      (Parser, skipSpace, char)
import Data.ByteString.Builder               (byteString)

import Data.Warc.Common                      (ToBuilder(..))
import Data.Warc.Key                         (Key, key)
import Data.Warc.Value                       (Value, value)
import Data.Warc.Shared                      (crlf)

data HeaderLine = HeaderLine Key Value

headerLine :: Parser HeaderLine
headerLine = do
    k <- key <* valueSeparator
    v <- value k <* crlf
    pure $ HeaderLine k v

valueSeparator :: Parser ()
valueSeparator = skipSpace >> char ':' >> skipSpace

getValue :: Key -> [HeaderLine] -> Maybe Value
getValue   _ [] = Nothing
getValue k (HeaderLine hk v:xs) | hk == k = Just v
                                | otherwise = getValue k xs 

instance ToBuilder HeaderLine where
    toBuilder (HeaderLine k v) = mconcat [ toBuilder k
                                         , byteString ": "
                                         , toBuilder v
                                         , byteString "\r\n" ]
