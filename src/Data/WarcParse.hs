{-# LANGUAGE OverloadedStrings #-}

module Data.WarcParse where

import Data.Attoparsec.ByteString.Lazy  as L (Parser, take, string, many1, Result(..), parse)
import Data.Attoparsec.ByteString.Char8      (choice, takeTill, takeWhile1, skipSpace, char)
import Data.Char                             (isSpace)
import Data.ByteString                       (ByteString)

import Data.Maybe                            (fromMaybe)

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as L8

data WarcEntry = WarcEntry !HeaderBlock !Body deriving Show

data HeaderBlock = HeaderBlock !WarcVersion ![HeaderLine] deriving Show

newtype WarcVersion = WarcVersion ByteString deriving Show

data HeaderLine = HeaderLine !Key !Value deriving Show

data Key = MandatoryKey !MandatoryKey 
         | OptionalKey !OptionalKey
         | CustomKey !CustomKey deriving Show

data MandatoryKey = WarcRecordId
                  | ContentLength
                  | WarcDate
                  | WarcType deriving Show

data OptionalKey = ContentType
                 | WarcTargetURI deriving Show

data CustomKey = CompressionMode
               | UncompressedContentLength 
               | UnknownKey !ByteString deriving Show

data Value = IntValue !Int
           | CompressionModeValue !CompressionMode
           | StringValue !ByteString deriving Show

data CompressionMode = Compressed
                     | Uncompressed deriving Show

data Body = CompressedBody !ByteString
          | UncompressedBody !ByteString deriving Show

data BodyDetails = BodyDetails !Int !CompressionMode

getBodyDetails :: HeaderBlock -> Maybe BodyDetails
getBodyDetails (HeaderBlock _ headers) = do
    contentLength <- fetchContentLength headers
    let compressionMode = fromMaybe Uncompressed (fetchCompressionMode headers)
    return $ BodyDetails contentLength compressionMode

    where
    fetchContentLength [] = Nothing
    fetchContentLength ((HeaderLine (MandatoryKey ContentLength) (IntValue i)):_) = Just i
    fetchContentLength (_:hs) = fetchContentLength hs

    fetchCompressionMode [] = Nothing
    fetchCompressionMode ((HeaderLine (CustomKey CompressionMode) (CompressionModeValue c)):_) = Just c
    fetchCompressionMode (_:hs) = fetchCompressionMode hs

crlf :: Parser ()
crlf = string "\r\n" *> pure ()

headerBlock :: Parser HeaderBlock
headerBlock = HeaderBlock <$> version <* crlf
                          <*> many1 headerLine

    where
    version :: Parser WarcVersion
    version = do
        _ <- string "WARC/"
        WarcVersion <$> takeTill isSpace 

    headerLine :: Parser HeaderLine
    headerLine = do
        k <- key <* valueSeparator
        v <- value k <* crlf
        pure $ HeaderLine k v

        where
        key :: Parser Key
        key = choice [ MandatoryKey <$> mandatoryKey
                     , OptionalKey <$> optionalKey
                     , CustomKey <$> customKey]

            where
            mandatoryKey = choice [ const WarcRecordId <$> string "WARC-Record-ID"
                                  , const ContentLength <$> string "Content-Length"
                                  , const WarcDate <$> string "WARC-Date"
                                  , const WarcType <$> string "WARC-Type" ]

            optionalKey = choice [ const ContentType <$> string "Content-Type"
                                 , const WarcTargetURI <$> string "WARC-Target-URI" ]

            customKey = choice [ const CompressionMode <$> string "Compression-Mode"
                               , const UncompressedContentLength <$> string "Uncompressed-Content-Length"
                               , UnknownKey <$> readKeyBytes ]

                where
                readKeyBytes :: Parser ByteString
                readKeyBytes = takeWhile1 (\x -> not (isSpace x || x == ':'))

        valueSeparator :: Parser ()
        valueSeparator = skipSpace >> char ':' >> skipSpace

        value :: Key -> Parser Value
        value (MandatoryKey ContentLength) = IntValue <$> int
        value (CustomKey CompressionMode) = CompressionModeValue <$> compressMode
        value (CustomKey UncompressedContentLength) = IntValue <$> int
        value _ = StringValue <$> takeWhile1 (not . (\x -> x == '\r' || x == '\n'))

        compressMode :: Parser CompressionMode
        compressMode = choice [ const Compressed <$> string "contentonly"
                              , const Uncompressed <$> string "none" ]

int :: Parser Int
int = do
    x <- takeWhile1 (not . isSpace)
    case C8.readInt x of
        Just (i,_) -> pure i
        Nothing    -> fail "meh" -- TODO <- fix

fromByteString :: L8.ByteString -> [WarcEntry]
fromByteString bs =
    case parse warcEntry bs of
        (Done remainder we) -> we : fromByteString remainder
        _ -> []

warcEntry :: Parser WarcEntry
warcEntry = do

    wh <- headerBlock

    crlf

    wb <- case getBodyDetails wh of
              Just bodyDetails -> body bodyDetails
              Nothing -> fail "meh" -- TODO <- fix

    crlf
    crlf

    pure $ WarcEntry wh wb

body :: BodyDetails -> Parser Body
body (BodyDetails sz Compressed) = CompressedBody <$> L.take sz
body (BodyDetails sz Uncompressed) = UncompressedBody <$> L.take sz
