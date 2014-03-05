VERSION=0.2.0.0

.PHONY: test
test:
	cabal build test
	./dist/build/test/test

clean:
	cabal clean
	cabal configure --enable-tests

release:
	cabal check
	cabal sdist
	git tag $(VERSION)
	git push --tags origin master
	cabal upload dist/hopfli-$(VERSION).tar.gz

SOURCES=$(wildcard src/cbits/*.c)
HEADERS=$(wildcard src/cbits/*.h)
tags: $(SOURCES) $(HEADERS)
	etags $^
