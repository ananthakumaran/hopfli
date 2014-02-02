{-# LANGUAGE RecordWildCards #-}

module Codec.Compression.Hopfli (
  Format(..)
  , compressWith
  , Codec.Compression.Hopfli.compress
  , defaultCompressOptions
) where

import           Codec.Compression.Hopfli.Raw as Raw
import           Data.ByteString
import           Foreign.C.Types              (CInt)

data VerboseLevel = NONE | VERBOSE | VERY_VERBOSE deriving (Show, Eq)

data CompressOptions = CompressOptions { verbose            :: VerboseLevel
                                       , numIterations      :: Int
                                       , blockSplitting     :: Bool
                                       , blockSplittingLast :: Bool
                                       , blockSplittingMax  :: Int }

defaultCompressOptions :: CompressOptions
defaultCompressOptions = CompressOptions { verbose = NONE, numIterations = 10, blockSplitting = True, blockSplittingLast = False, blockSplittingMax = 15 }

defaultFormat :: Format
defaultFormat = ZLIB

boolToInt :: Bool -> CInt
boolToInt True = 1
boolToInt False = 0

toOptions :: CompressOptions -> Options
toOptions (CompressOptions {..})
  = Options verbose' verboseMore (fromIntegral numIterations) (boolToInt blockSplitting) (boolToInt blockSplittingLast) (fromIntegral blockSplittingMax)
  where verbose' = boolToInt $ VERBOSE == verbose || VERY_VERBOSE == verbose
        verboseMore = boolToInt $ VERY_VERBOSE == verbose

compressWith :: CompressOptions -> Format -> ByteString -> ByteString
compressWith options = Raw.compress (toOptions options)

compress :: ByteString -> ByteString
compress = compressWith defaultCompressOptions defaultFormat
