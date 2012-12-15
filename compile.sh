#!/bin/sh
ghc -O2 -threaded -hide-package monads-tf *hs -main-is Villain.Game.main
