# Hopfli [![Build Status](https://secure.travis-ci.org/ananthakumaran/hopfli.png)](http://travis-ci.org/ananthakumaran/hopfli)

*Zopfli Compression Algorithm is a new zlib (gzip, deflate) compatible
compressor. This compressor takes more time (~100x slower), but
compresses around 5% better than zlib and better than any other
zlib-compatible compressor we have found.*

Hopfli provides a pure interface to compress data using Zopfli algorithm.


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
