Hopfli - Haskell bindings to the Zopfli library
===
[![Build Status](https://secure.travis-ci.org/ananthakumaran/hopfli.svg)](https://travis-ci.org/ananthakumaran/hopfli.svg?branch=master)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/hopfli.svg)](http://packdeps.haskellers.com/specific?package=hopfli)
[![Hackage](https://img.shields.io/hackage/v/hopfli.svg)](https://hackage.haskell.org/package/hopfli)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)


Hopfli provides a pure interface to compress data using the Zopfli library.

*Zopfli is a compression library released by Google in 2013, which can output
either a raw DEFLATE stream, or one wrapped into zlib or gzip formats. Under
default settings, the output produced by Zopfli is 3.7–8.3% smaller than that of
`gzip -9`, though the algorithm is 81 times slower.*

Zopfli is distributed under the Apache 2.0 license.

## Example

````haskell
import           Codec.Compression.Hopfli
import           Data.ByteString
import           System.IO                (stdin, stdout)

main :: IO ()
main = hGetContents stdin >>= hPut stdout . compressWith defaultCompressOptions GZIP
````

````
runGhc example.hs < README.md > README.md.gz
````
