{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings, RecordWildCards #-}

module Codec.Compression.Hopfli.Raw (
  Options(Options)
  , Format(..)
  , compress
) where

import qualified Codec.Compression.GZip     as G
import qualified Codec.Compression.Zlib     as Z
import qualified Codec.Compression.Zlib.Raw as ZR
import qualified Data.ByteString            as B
import           Data.ByteString.Char8      ()
import           Data.ByteString.Internal   (fromForeignPtr, toForeignPtr)
import qualified Data.ByteString.Lazy       as BL
import           Data.ByteString.Lazy.Char8 ()
import           Foreign                    hiding (unsafePerformIO)
import           Foreign.C.Types
import           Prelude                    hiding (length)
import           System.IO.Unsafe           (unsafePerformIO)

#include "zopfli.h"

data Options = Options { verbose            :: CInt
                       , verbose_more       :: CInt
                       , numiterations      :: CInt
                       , blocksplitting     :: CInt
                       , blocksplittinglast :: CInt
                       , blocksplittingmax  :: CInt } deriving (Show)

instance Storable Options where
    sizeOf    _ = (#size ZopfliOptions)
    alignment _ = alignment (undefined :: CInt)
    peek ptr = do
        verbose <- (#peek ZopfliOptions, verbose) ptr
        verbose_more <- (#peek ZopfliOptions, verbose_more) ptr
        numiterations <- (#peek ZopfliOptions, numiterations) ptr
        blocksplitting <- (#peek ZopfliOptions, blocksplitting) ptr
        blocksplittinglast <- (#peek ZopfliOptions, blocksplittinglast) ptr
        blocksplittingmax <- (#peek ZopfliOptions, blocksplittingmax) ptr
        return $ Options{..}
    poke ptr (Options verbose' verbose_more' numiterations' blocksplitting' blocksplittinglast' blocksplittingmax') = do
        (#poke ZopfliOptions, verbose) ptr verbose'
        (#poke ZopfliOptions, verbose_more) ptr verbose_more'
        (#poke ZopfliOptions, numiterations) ptr numiterations'
        (#poke ZopfliOptions, blocksplitting) ptr blocksplitting'
        (#poke ZopfliOptions, blocksplittinglast) ptr blocksplittinglast'
        (#poke ZopfliOptions, blocksplittingmax) ptr blocksplittingmax'

data Format = GZIP | ZLIB | DEFLATE deriving (Show, Eq)

newtype ZopfliFormat = ZopfliFormat CInt

fromFormat :: Format -> ZopfliFormat
fromFormat GZIP = ZopfliFormat #{const ZOPFLI_FORMAT_GZIP}
fromFormat ZLIB = ZopfliFormat #{const ZOPFLI_FORMAT_ZLIB}
fromFormat DEFLATE = ZopfliFormat #{const ZOPFLI_FORMAT_DEFLATE}


foreign import ccall unsafe "zopfli.h ZopfliCompress"
  c_zopfli_compress :: Ptr Options -> ZopfliFormat -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()


compress :: Options -> Format -> B.ByteString -> B.ByteString
-- zopfli doesn't handle zero length data
compress _ GZIP "" = B.concat . BL.toChunks $ G.compress ""
compress _ ZLIB "" = B.concat . BL.toChunks $ Z.compress ""
compress _ DEFLATE "" = B.concat . BL.toChunks $ ZR.compress ""

compress options format input = unsafePerformIO $ do
  let (inputFptr, start, length) = toForeignPtr input
  withForeignPtr inputFptr $ \inputPtr -> do
    alloca $ \outSizePtr -> do
      poke outSizePtr 0
      alloca $ \optionsPtr -> do
        poke optionsPtr options
        alloca $ \outPtrPtr -> do
          poke outPtrPtr nullPtr
          _ <- c_zopfli_compress optionsPtr (fromFormat format) (plusPtr inputPtr start) (fromIntegral length) outPtrPtr outSizePtr
          outSize <- peek outSizePtr
          resultPtr <- throwIfNull "Zopfli compression failed" (peek outPtrPtr)
          resultFptr <- newForeignPtr finalizerFree resultPtr
          return $ fromForeignPtr resultFptr 0 (fromIntegral outSize)

