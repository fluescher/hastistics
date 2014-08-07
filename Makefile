all: build

clean:
	@cabal clean

configure: clean
	@cabal install --enable-tests
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
	ghci src/Hastistics.hs src/Hastistics/Fields.hs src/Hastistics/Types.hs src/Hastistics/Data/CSV.hs src/Hastistics/Distributions.hs
