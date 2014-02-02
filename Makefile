.PHONY: test
test:
	cabal build test
	./dist/build/test/test

clean:
	cabal clean
	cabal configure --enable-tests

SOURCES=$(wildcard src/cbits/*.c)
HEADERS=$(wildcard src/cbits/*.h)
tags: $(SOURCES) $(HEADERS)
	etags $^
