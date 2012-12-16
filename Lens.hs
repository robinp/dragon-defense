{-# LANGUAGE TemplateHaskell #-}

module Villain.Logic.Lens where

import Control.Lens

import Villain.Logic

makeLenses ''Bomb
makeLenses ''Peg
makeLenses ''Hanger
makeLenses ''Attacker
makeLenses ''Level
makeLenses ''Rect
