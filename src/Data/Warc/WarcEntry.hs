{-# LANGUAGE OverloadedStrings
           , DeriveGeneric     #-}

module Data.Warc.WarcEntry where

import           Control.DeepSeq
import           Data.Attoparsec.ByteString.Lazy (Parser)
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString         as BS   (length)
import qualified Data.ByteString.Builder as B    (toLazyByteString, byteString)
import           Data.ByteString.Lazy            (toStrict)
import qualified Data.ByteString.Lazy    as Lazy (ByteString)
import           GHC.Generics                    (Generic)

import           Data.Warc.Common       (ToBuilder (..))
import           Data.Warc.Body 
import           Data.Warc.Header
import           Data.Warc.Shared       (crlf)
import           Data.Warc.Key
import           Data.Warc.Value

data WarcEntry = WarcEntry !WarcHeader !WarcBody
                   deriving Generic
instance NFData WarcEntry

warcEntry :: Parser WarcEntry
warcEntry = do

    header <- warcHeader

    crlf

    body <- do
        contentLength <- getContentLength header
        compressionMode <- getCompressionMode header
        warcbody contentLength compressionMode

    crlf
    crlf

    pure $ WarcEntry header body

    where
    getContentLength :: WarcHeader -> Parser Int
    getContentLength header = case getValue (MandatoryKey ContentLength) header of
        Just (IntValue i) -> pure i
        Nothing           -> fail "Could not find content-length"

    getCompressionMode :: WarcHeader -> Parser CompressionMode
    getCompressionMode header = case getValue (CustomKey CompressionMode) header of
        Just (CompressionModeValue c) -> pure c
        Nothing                       -> pure Uncompressed

compress :: WarcEntry -> WarcEntry
compress w@(WarcEntry _ (CompressedBody _)) = w
compress (WarcEntry headers ub@(UncompressedBody b)) =
    let uncompressedLength = BS.length b
        cb@(CompressedBody cb') = compressBody ub
        compressedLength = BS.length cb'
        headers' = setValue (MandatoryKey ContentLength) (Just (IntValue compressedLength))
                 . setValue (CustomKey UncompressedContentLength) (Just (IntValue uncompressedLength))
                 . setValue (CustomKey CompressionMode) (Just (CompressionModeValue Compressed))
                 $ headers
    in WarcEntry headers' cb

decompress :: WarcEntry -> WarcEntry
decompress w@(WarcEntry _ (UncompressedBody _)) = w
decompress   (WarcEntry headers cb@(CompressedBody _)) =
    let ub@(UncompressedBody ub') = decompressBody cb
        uncompressedLength = BS.length ub'
        headers' = setValue (MandatoryKey ContentLength) (Just (IntValue uncompressedLength))
                 . setValue (CustomKey UncompressedContentLength) Nothing
                 . setValue (CustomKey CompressionMode) Nothing
                 $ headers
    in WarcEntry headers' ub

toByteString :: WarcEntry -> ByteString
toByteString = toStrict . B.toLazyByteString . toBuilder

toLazyByteString :: WarcEntry -> Lazy.ByteString
toLazyByteString = B.toLazyByteString . toBuilder

instance ToBuilder WarcEntry where
    toBuilder (WarcEntry header body) =
        mconcat [ toBuilder header
                , B.byteString "\r\n"
                , toBuilder body
                , B.byteString "\r\n\r\n"]
