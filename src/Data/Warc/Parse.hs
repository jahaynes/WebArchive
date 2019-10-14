module Data.Warc.Parse where

import Data.Warc.WarcEntry

import           Data.Attoparsec.ByteString.Lazy      (Result(..), parse)
import           Data.Attoparsec.Internal.Types       (Parser (Parser), Pos (Pos))
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Lazy            as L

fromByteString :: L.ByteString -> [Either String WarcEntry]
fromByteString bs
    | L.null bs = []
    | otherwise = case parse warcEntry bs of
                      (Done remainder we) -> Right we : fromByteString remainder
                      _ -> [Left "error"]

fromByteStringRemainder :: L.ByteString -> Either String (Int, WarcEntry, L.ByteString)
fromByteStringRemainder bs
    | L.null bs = Left "no data"
    | otherwise = case parse warcEntryPos bs of
                      (Done remainder (pos, we)) -> Right (pos, we, remainder)
                      _                          -> Left "error"

warcEntryPos :: Parser ByteString (Int, WarcEntry)
warcEntryPos = do
    we  <- warcEntry
    pos <- getPos
    pure (pos, we)

getPos :: Parser a Int
getPos = Parser $ \t p@(Pos pos) more _lose suc -> suc t p more pos
