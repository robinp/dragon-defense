module Villain.Game (main) where

import GHC.Float

import Control.Applicative
import Control.Lens

import Graphics.Gloss

import Villain.Logic
import Villain.Logic.Lens

drawLevel l = Pictures $ drawHang <$> l^.hangs

drawHang h = 
    let biD2F = both %~ double2Float
        (px, py) = biD2F $ h^.pegH.posP
        (bx, by) = biD2F $ h^.bombH.posB
        peg   = Translate px py $ Color blue $ Circle 10
        rope  = Line [(px, py), (bx, by)]
        bomb  = Translate bx by $ Color red $ drawRect $ bboxOf (h^.bombH.kindB)
    in
      Pictures [rope, peg, bomb]

drawRect :: Rect -> Picture
drawRect (Rect tl br) = Polygon $ mapped.both %~ double2Float $ [tl, yx tl br, br, xy tl br, tl]
  where xy (x, _) (_, y) = (x, y)
        yx (_, y) (x, _) = (x, y)

initLevel = Level [Hanger (Peg (100, 200)) (Bomb Cow (100, 100) 0)] [] []

main :: IO ()
main = display (InWindow "Liter" (640, 480) (10, 10)) white $ drawLevel initLevel
