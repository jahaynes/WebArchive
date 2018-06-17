{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Data.Warc.Header where

import Data.Attoparsec.ByteString.Lazy  (Parser, many1, string)
import Data.ByteString.Builder          (toLazyByteString, byteString)
import Data.ByteString                  (ByteString)
import Data.Char                        (isSpace)
import GHC.Generics                     (Generic)

import           Data.Warc.Common
import           Data.Warc.Key              (Key)
import           Data.Warc.Value            (Value)
import           Data.Warc.HeaderLine       (HeaderLine (..), headerLine)
import qualified Data.Warc.HeaderLine as HL (getValue)
import           Data.Warc.Shared           (takeTill1, crlf)

data WarcHeader = WarcHeader {-# UNPACK #-} !WarcVersion [HeaderLine] deriving Generic

newtype WarcVersion = WarcVersion ByteString deriving Generic

getValue :: Key -> WarcHeader -> Maybe Value
getValue key (WarcHeader _ headers) = HL.getValue key headers

setValue :: Key -> Maybe Value -> WarcHeader -> WarcHeader
setValue key    Nothing (WarcHeader ver headers) = WarcHeader ver (filter (\(HeaderLine k _) -> not (buildEq k key)) headers)
setValue key (Just val) (WarcHeader ver headers) = WarcHeader ver (go headers)
    where
    go [] = [HeaderLine key val]
    go (h@(HeaderLine k v):hs)
        | key == k  = HeaderLine key val : hs
        | otherwise = h : go hs 

buildEq a b = (toLazyByteString . toBuilder) a == (toLazyByteString . toBuilder) b  --TODO

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
                , byteString "\r\n"] ++ map toBuilder headerLines   --TODO
