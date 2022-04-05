#!/bin/sh

cabal clean
cabal configure 
cabal haddock --executables --tests --benchmarks --all --internal

