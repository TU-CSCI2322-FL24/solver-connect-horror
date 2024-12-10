# Commands:

.PHONY: build prof all clean setup

# Build with optimization
build:
	ghc --make -O -i./src -o connect4 src/Main.hs

# Build with profiling
prof:
	ghc --make -prof -i./src -o connect4 src/Main.hs

# Default target
all: build

# Cleaning commands
clean:
	rm -f connect4
	rm -f src/*.hi
	rm -f src/*.o

# Setup dependencies (if needed)
setup:
	if [ ! -d "/users/sfogarty" ]; then	cabal update; fi
	if [ ! -d "/users/sfogarty" ]; then	cabal v1-install ansi-terminal; fi
	if [ ! -d "/users/sfogarty" ]; then  cabal v1-install drawille; fi
