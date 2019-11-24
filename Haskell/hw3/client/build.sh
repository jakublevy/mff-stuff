#!/bin/sh

mkdir -p build
cwd=$(pwd)
cd build
stack exec -- ghc -threaded ../src/Main.hs ../src/MainBrick.hs ../src/Writer.hs ../src/Game.hs ../src/GameTypes.hs  -odir . -hidir . -o gol
cd $cwd
