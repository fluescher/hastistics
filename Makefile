all: build

clean:
	@cabal clean

configure: clean
	@cabal configure --enable-tests

build: configure
	@cabal build

test: build
	@cabal test

install:
	@cabal install --user

haddock: configure
	@cabal haddock

repl:
	ghci src/Hastistics.hs src/Hastistics/Data.hs
