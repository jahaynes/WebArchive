{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.Shared where

import           Data.Attoparsec.ByteString.Char8      (takeWhile1, isSpace)
import           Data.Attoparsec.ByteString.Lazy       (Parser, string)
import qualified Data.Attoparsec.Internal.Types   as I (Parser)
import           Data.ByteString.Char8                 (ByteString, readInt)
import           Data.Functor                          (($>))

{-# INLINE crlf #-}
crlf :: Parser ()
crlf = string "\r\n" $> ()

{-# INLINE takeTill1 #-}
takeTill1 :: (Char -> Bool) -> Parser ByteString
takeTill1 f = takeWhile1 $ not . f

--Parses 'raw', ignores the output, and replace with a 'tag'
{-# INLINE (<%>) #-}
(<%>) :: b -> ByteString -> I.Parser ByteString b
(<%>) tag raw = const tag <$> string raw

{-# INLINE int #-}
int :: ByteString -> Parser Int
int x = case readInt x of
    Just (i,_) -> pure i
    Nothing    -> fail ("Expected integer value" ++ show x)

{-# INLINE intThenSpace #-}
intThenSpace :: Parser Int
intThenSpace = int =<< takeTill1 isSpace
