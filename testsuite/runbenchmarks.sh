#!/bin/sh

cabal clean
cabal configure --enable-benchmarks
cabal build
cabal bench

