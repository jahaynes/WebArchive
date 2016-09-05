{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.Header.Value where

import Data.Attoparsec.ByteString.Char8      (Parser, choice, takeWhile1, isSpace, char)
import Data.ByteString.Builder               (Builder, toLazyByteString, byteString, intDec, char8)
import Data.ByteString.Char8                 (ByteString, readInt)
import Data.ByteString.Lazy                  (toStrict)
import Data.Warc.Header.Key   hiding (build)
import Data.Warc.Shared

data Date = Date 
           { year ::    {-# UNPACK #-}  !Int
           , month ::   {-# UNPACK #-}  !Int 
           , day ::     {-# UNPACK #-}  !Int
           , hour ::    {-# UNPACK #-}  !Int
           , min ::     {-# UNPACK #-}  !Int
           , sec ::     {-# UNPACK #-}  !Int } 

data Value = IntValue             {-# UNPACK #-} !Int
           | CompressionModeValue                !CompressionMode
           | DateValue            {-# UNPACK #-} !Date
           | StringValue          {-# UNPACK #-} !ByteString

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
    intThenSkipChar cAfter = int =<< takeTill1 (==cAfter) <* char cAfter

{-# INLINE int #-}
int :: ByteString -> Parser Int
int x = case readInt x of
    Just (i,_) -> pure i
    Nothing    -> fail ("Expected integer value" ++ show x)

compressMode :: Parser CompressionMode
compressMode = choice [ Compressed <%> "contentonly"
                      , Uncompressed <%> "none" ]

build :: Value -> Builder
build (CompressionModeValue Compressed) = byteString "contentonly"
build (CompressionModeValue Uncompressed) = byteString "none"
build (IntValue i) = intDec i
build (StringValue bs) = byteString bs
build (DateValue (Date yr mo da hr mi sc)) =
    mconcat [ intDec yr, char8 '-'
            , intDec mo, char8 '-'
            , intDec da, char8 'T'
            , intDec hr, char8 ':'
            , intDec mi, char8 ':'
            , intDec sc, char8 'Z' ]

