module Data.Warc.Common where

import Data.ByteString.Builder (Builder)

class ToBuilder a where
    toBuilder :: a -> Builder
