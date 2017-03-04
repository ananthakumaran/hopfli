{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HopfliSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import qualified Codec.Compression.GZip     as GZip
import           Codec.Compression.Hopfli
import qualified Codec.Compression.Zlib     as Zlib
import qualified Codec.Compression.Zlib.Raw as ZRaw
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

toLazy :: B.ByteString -> BL.ByteString
toLazy str = BL.fromChunks  [str]

instance Arbitrary B.ByteString where
  arbitrary = B.pack <$> arbitrary

instance CoArbitrary B.ByteString where
  coarbitrary = coarbitrary . B.unpack

makeStrict :: (BL.ByteString -> BL.ByteString) -> B.ByteString -> B.ByteString
makeStrict decompressor = toStrict . decompressor . toLazy

spec :: Spec
spec =
  describe "compress" $ do
    it "Compresses in zlib compatible format" . property $
      \payload -> (makeStrict Zlib.decompress . compress $ payload) == payload
    it "Compresses in gzip compatible format" . property $
      \payload -> (makeStrict GZip.decompress . compressWith defaultCompressOptions GZIP $ payload) == payload
    it "Compresses in deflate compatible format" . property $
      \payload -> (makeStrict ZRaw.decompress . compressWith defaultCompressOptions DEFLATE $ payload) == payload
