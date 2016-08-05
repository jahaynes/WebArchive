{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.Header.Value where

import Data.Attoparsec.ByteString.Char8      (Parser, choice, char, takeWhile1, isSpace)
import Data.ByteString.Char8                 (ByteString, readInt, pack)

import Data.Warc.Header.Key
import Data.Warc.Shared

data Date = Date
           { year :: Int
           , month :: Int 
           , day :: Int
           , hour :: Int
           , min :: Int
           , sec :: Int } deriving (Eq, Show)

data Value = IntValue Int
           | CompressionModeValue CompressionMode
           | DateValue Date
           | StringValue ByteString deriving (Eq, Show)

value :: Key -> Parser Value
value (MandatoryKey ContentLength) = IntValue <$> intThenSpace
value (MandatoryKey WarcDate) = DateValue <$> date
value (CustomKey CompressionMode) = CompressionModeValue <$> compressMode
value (CustomKey UncompressedContentLength) = IntValue <$> intThenSpace
value _ = StringValue <$> takeTill1 (\x -> x == '\r' || x == '\n')

intThenSpace :: Parser Int
intThenSpace = int =<< takeTill1 isSpace

date :: Parser Date
date = Date <$> intThenSkipChar '-'
            <*> intThenSkipChar '-'
            <*> intThenSkipChar 'T'
            <*> intThenSkipChar ':'
            <*> intThenSkipChar ':'
            <*> intThenSkipChar 'Z'
    where
    intThenSkipChar :: Char -> Parser Int
    intThenSkipChar cAfter = int =<< takeTill1 (==cAfter) <* takeWhile1 (==cAfter)

int :: ByteString -> Parser Int
int x = case readInt x of
    Just (i,_) -> pure i
    Nothing    -> fail ("Expected integer value" ++ show x)

compressMode :: Parser CompressionMode
compressMode = choice [ Compressed <%> "contentonly"
                      , Uncompressed <%> "none" ]

toByteString :: Value -> ByteString
toByteString (CompressionModeValue Compressed) = "contentonly"
toByteString (CompressionModeValue Uncompressed) = "none"
toByteString (DateValue (Date yr mnt day hr mnu sec)) = let [a,b,c,d,e,f] = map show [yr, mnt, day, hr, mnu, sec]
    in pack $ concat [a,"-",b,"-",c,"T",d,":",e,":",f,"Z"]
toByteString (IntValue i) = pack . show $ i
toByteString (StringValue bs) = bs
