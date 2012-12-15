module Villain.Game (main) where

import GHC.Float

import Control.Applicative
import Control.Lens

import Graphics.Gloss

import Villain.Logic
import Villain.Logic.Lens

drawLevel l = Pictures $ drawHangs ++ drawAks ++ [drawNotify]
  where drawHangs = drawHang <$> l^.hangs
        drawAks = drawAk <$> l^.aks

drawNotify = Translate (-250) 200 $ Scale 0.15 0.15 $ Text "Working on attacker progression"

drawHang h = 
    let (px, py) = biD2F $ h^.pegH.posP
        (bx, by) = biD2F $ h^.bombH.posB
        peg   = Translate px py $ Color red $ ThickCircle 10 3
        rope  = Line [(px, py), (bx, by)]
        bomb  = Translate bx by $ Color red $ drawRect $ bboxOf (h^.bombH.kindB)
    in
      Pictures [rope, peg, bomb]


biD2F = both %~ double2Float

drawAk ak =
    let (x, y) = biD2F $ ak^.posA
        col = case ak^.kindA of
          Knight      -> green
          FastKnight  -> blue
    in
      Color col $ Translate x y $ ThickCircle 10 5  

drawRect :: Rect -> Picture
drawRect (Rect tl br) = Polygon $ mapped.both %~ double2Float $ [tl, yx tl br, br, xy tl br, tl]
  where xy (x, _) (_, y) = (x, y)
        yx (_, y) (x, _) = (x, y)

initLevel = Level hs [] aks
  where hs  = [Hanger (Peg (100, 200)) (Bomb Cow (100, 100) 0)]
        aks = [Attacker Knight (-100, ky), Attacker FastKnight (-150, ky)]
        ky = -100

main :: IO ()
main = display (InWindow "Liter" (640, 480) (10, 10)) white $ drawLevel initLevel
