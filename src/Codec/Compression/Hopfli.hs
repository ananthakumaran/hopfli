{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hopfli
-- License     : Apache 2.0
-- Copyright   : (c) 2014 Anantha Kumaran
-- Stability   : experimental
-- Portability : unknown
--
-----------------------------------------------------------------------------
module Codec.Compression.Hopfli (
    Format(..)
  , CompressOptions(..)
  , VerboseLevel(..)
  , compressWith
  , Codec.Compression.Hopfli.compress
  , defaultCompressOptions
  , defaultFormat
  ) where

import           Codec.Compression.Hopfli.Raw as Raw
import           Data.ByteString
import           Data.Bool (bool)
import           Foreign.C.Types              (CInt)

data VerboseLevel = NONE
                  | VERBOSE
                  | VERY_VERBOSE
  deriving (Show, Eq)

data CompressOptions = CompressOptions { verbose            :: VerboseLevel
                                       , numIterations      :: Int
                                       , blockSplitting     :: Bool
                                       , blockSplittingLast :: Bool
                                       , blockSplittingMax  :: Int
                                       }

defaultCompressOptions :: CompressOptions
defaultCompressOptions =
  CompressOptions { verbose            = NONE
                  , numIterations      = 10
                  , blockSplitting     = True
                  , blockSplittingLast = False
                  , blockSplittingMax  = 15
                  }

defaultFormat :: Format
defaultFormat = ZLIB

toOptions :: CompressOptions -> Options
toOptions CompressOptions{..} =
    Options verbose' verboseMore numIterations' blockSplitting' blockSplittingLast' blockSplittingMax'
  where verbose'            = boolToInt $ VERBOSE == verbose || VERY_VERBOSE == verbose
        verboseMore         = boolToInt $ VERY_VERBOSE == verbose
        numIterations'      = fromIntegral numIterations
        blockSplitting'     = boolToInt blockSplitting
        blockSplittingLast' = boolToInt blockSplittingLast
        blockSplittingMax'  = fromIntegral blockSplittingMax
        boolToInt           = bool 0 1 :: Bool -> CInt

compressWith :: CompressOptions -> Format -> ByteString -> ByteString
compressWith options = Raw.compress (toOptions options)

compress :: ByteString -> ByteString
compress = compressWith defaultCompressOptions defaultFormat
