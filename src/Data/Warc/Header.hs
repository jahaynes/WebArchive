{-# LANGUAGE OverloadedStrings #-}

module Data.Warc.Header where

import Data.Attoparsec.ByteString.Lazy  (Parser, many1, string)
import Data.ByteString.Builder          (byteString)
import Data.ByteString                  (ByteString)
import Data.Char                        (isSpace)
import Data.Maybe                       (mapMaybe)

import           Data.Warc.Common
import           Data.Warc.Key              (Key)
import           Data.Warc.Value            (Value)
import           Data.Warc.HeaderLine       (HeaderLine (..), headerLine)
import qualified Data.Warc.HeaderLine as HL (getValue)
import           Data.Warc.Shared           (takeTill1, crlf)

data WarcHeader = WarcHeader {-# UNPACK #-} !WarcVersion [HeaderLine]

newtype WarcVersion = WarcVersion ByteString

getValue :: Key -> WarcHeader -> Maybe Value
getValue key (WarcHeader _ headers) = HL.getValue key headers

setValue :: Key -> Maybe Value -> WarcHeader -> WarcHeader
setValue key mVal (WarcHeader ver headers) =
    WarcHeader ver (mapMaybe change headers)
    where
    change :: HeaderLine -> Maybe HeaderLine
    change h@(HeaderLine k _) | k == key  = HeaderLine k <$> mVal
                              | otherwise = Just h

warcHeader :: Parser WarcHeader
warcHeader = WarcHeader <$> version
                        <*  crlf
                        <*> many1 headerLine

version :: Parser WarcVersion
version = WarcVersion <$> (string "WARC/" *> takeTill1 isSpace)

instance ToBuilder WarcHeader where
    toBuilder (WarcHeader (WarcVersion ver) headerLines) =
      mconcat $ [ byteString "WARC/"
                , byteString ver
                , byteString "\r\n"] ++ map toBuilder headerLines
