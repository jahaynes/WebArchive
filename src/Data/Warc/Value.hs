{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.Value where

import Data.Attoparsec.ByteString.Char8      (Parser, choice, char)
import Data.ByteString.Builder               (byteString, intDec, char8)
import Data.ByteString.Char8                 (ByteString)

import Data.Warc.Common
import Data.Warc.Key
import Data.Warc.Shared

data Date = Date 
          { year  :: {-# UNPACK #-} !Int
          , month :: {-# UNPACK #-} !Int 
          , day   :: {-# UNPACK #-} !Int
          , hour  :: {-# UNPACK #-} !Int
          , min   :: {-# UNPACK #-} !Int
          , sec   :: {-# UNPACK #-} !Int } 

data CompressionMode = Compressed
                     | Uncompressed

data Value = IntValue             {-# UNPACK #-} !Int
           | CompressionModeValue                !CompressionMode
           | DateValue            {-# UNPACK #-} !Date
           | StringValue          {-# UNPACK #-} !ByteString

instance ToBuilder Value where
    toBuilder (CompressionModeValue Compressed) = byteString "contentonly"
    toBuilder (CompressionModeValue Uncompressed) = byteString "none"
    toBuilder (IntValue i) = intDec i
    toBuilder (StringValue bs) = byteString bs
    toBuilder (DateValue (Date yr mo da hr mi sc)) =
        mconcat [ intDec yr, char8 '-'
                , intDec mo, char8 '-'
                , intDec da, char8 'T'
                , intDec hr, char8 ':'
                , intDec mi, char8 ':'
                , intDec sc, char8 'Z' ]    

value :: Key -> Parser Value
value (MandatoryKey ContentLength) = IntValue <$> intThenSpace
value (MandatoryKey WarcDate) = DateValue <$> date
value (CustomKey CompressionMode) = CompressionModeValue <$> compressMode
value (CustomKey UncompressedContentLength) = IntValue <$> intThenSpace
value _ = StringValue <$> takeTill1 (\x -> x == '\r' || x == '\n')

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

compressMode :: Parser CompressionMode
compressMode = choice [ Compressed <%> "contentonly"
                      , Uncompressed <%> "none" ]
