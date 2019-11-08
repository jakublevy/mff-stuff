#!/bin/sh

mkdir -p build
cwd=$(pwd)
cd build
stack exec -- ghc ../src/Main.hs ../src/TokenParser.hs ../src/StringParser.hs ../src/State.hs ../src/Parser.hs ../src/SlepysParser.hs ../src/SlepysLexer.hs ../src/SlepysPrettifier.hs ../src/SymbolTable.hs ../src/SlepysSemantic.hs ../src/Common.hs -odir . -hidir . -o slepys
cd $cwd
