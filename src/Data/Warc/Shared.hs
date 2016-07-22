{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.Shared where

import Data.Attoparsec.ByteString.Lazy          (Parser, string)
import qualified Data.Attoparsec.Internal.Types as I (Parser)
import Data.Attoparsec.ByteString.Char8         (takeWhile1)
import Data.ByteString                          (ByteString)

data CompressionMode = Compressed
                     | Uncompressed deriving Show

--Carriage return - Line feed
crlf :: Parser ()
crlf = string "\r\n" *> pure ()

takeTill1 :: (Char -> Bool) -> Parser ByteString
takeTill1 f = takeWhile1 $ not . f

--Parses 'raw', ignores the output, and replace with a 'tag'
(<%>) :: b -> ByteString -> I.Parser ByteString b
(<%>) tag raw = const tag <$> string raw
