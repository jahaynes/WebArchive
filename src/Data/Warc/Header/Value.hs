{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.Header.Value where

import Data.Attoparsec.ByteString.Char8      (Parser, choice)
import Data.Char                             (isSpace)
import Data.ByteString.Char8                 (ByteString, readInt)

import Data.Warc.Header.Key
import Data.Warc.Shared

data Value = IntValue Int
           | CompressionModeValue CompressionMode
           | StringValue ByteString deriving Show

value :: Key -> Parser Value
value (MandatoryKey ContentLength) = IntValue <$> int
value (CustomKey CompressionMode) = CompressionModeValue <$> compressMode
value (CustomKey UncompressedContentLength) = IntValue <$> int
value _ = StringValue <$> takeTill1 (\x -> x == '\r' || x == '\n')

int :: Parser Int
int = do
    x <- takeTill1 isSpace
    case readInt x of
        Just (i,_) -> pure i
        Nothing    -> fail "meh" -- TODO <- fix

compressMode :: Parser CompressionMode
compressMode = choice [ Compressed <%> "contentonly"
                      , Uncompressed <%> "none" ]
