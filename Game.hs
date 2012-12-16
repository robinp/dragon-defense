{-# LANGUAGE TemplateHaskell #-}

module Villain.Game (main) where

import GHC.Float

import Control.Applicative
import Control.Lens
import Control.Monad.State

import Data.List (find, partition)

import Graphics.Gloss

import RWSExtras

import Villain.Logic
import Villain.Logic.Lens

import Debug.Trace

drawLevel lvl = Pictures $ drawHangs ++ drawAks ++ drawBombs ++ [drawNotify]
  where drawHangs = drawHang <$> lvl^.hangs
        drawAks = drawAk <$> lvl^.aks
        drawBombs = drawBomb <$> lvl^.falls

drawNotify = Translate (-250) 200 $ Scale 0.15 0.15 $ Text "Enemy collision"

drawBomb b =
  let (x, y) = bombXY b
  in  Translate x y $ Color red $ drawRect $ bboxOf (b^.kindB)

bombXY = biD2F . view posB

drawHang h = 
    let (px, py) = biD2F $ h^.pegH.posP
        (bx, by) = bombXY $ h^.bombH
        peg   = Translate px py $ Color red $ ThickCircle 10 3
        rope  = Line [(px, py), (bx, by)]
        bomb  = drawBomb $ h^.bombH
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
  where hs  = [
          Hanger (Peg (100, 200)) (Bomb Cow (100, 100) 0),
          Hanger (Peg (10, 180)) (Bomb Anchor (10, 130) 0) ]
        aks = [Attacker Knight (-100, ky), Attacker FastKnight (-150, ky)]
        ky = -100

main :: IO ()
main = play (InWindow "Liter" (640, 480) (10, 10)) white fps initLevel drawLevel input update
  where input _ = execState (detachFirst $ const True)
        update _ w = (bombAks . moveBombs . moveAks) w
        fps = 20

detachFirst :: (Hanger -> Bool) -> State Level ()
detachFirst p = do
  mayH <- withSubSt hangs $ popHanger p
  falls %= (maybe [] (return . _bombH) mayH ++)

popHanger :: (Hanger -> Bool) -> State [Hanger] (Maybe Hanger)
popHanger p = do
  mayHanger <- gets $ find p
  modify $ removeFst p
  return mayHanger
  where
    removeFst p xs = 
      let (as, bs) = span (not . p) xs
      in  as ++ if null bs then bs else tail bs

moveAks :: Level -> Level
moveAks = aks.mapped %~ moveAk
  where moveAk = do
          k <- view kindA
          posA._1 +~ akSpeed k  

bombAks :: Level -> Level
bombAks lvl = 
  let (remBs, remAks) = bombAks0 (lvl^.falls) (lvl^.aks) []
  in  flip execState lvl $ (falls .= remBs) >> (aks .= remAks) -- TODO Lens & ?

bombAks0 :: [Bomb] -> [Attacker] -> [Bomb] -> ([Bomb], [Attacker])
bombAks0 bombs aks remainBombs = case bombs of
  []    ->  (reverse remainBombs, aks)
  b:bs  ->  let (aksHit, aksNotHit) = partition (hitBy b) aks
                rb = if null aksHit then b:remainBombs else remainBombs
            in  bombAks0 bs aksNotHit rb

hitBy b ak = 
  --TODO
  near (b^.posB) (ak^.posA) 50
  where
    near (x1, y1) (x2, y2) d = abs (x1 - x2) + abs (y1 - y2) <= d

moveBombs :: Level -> Level
moveBombs = disappearBombs . fallBombs
  where disappearBombs = falls %~ filter (views (posB._2) (floorLine <))
                               -- filter (\b -> b^.posB._2 > floorLine) -- is this simpler?
        floorLine = (-150)
        fallBombs = falls.mapped %~ execState fallBomb
        fallBomb = do
          spd <- speedB <+= 0.15 -- gravity
          posB._2 -= spd

akSpeed :: AttackerKind -> Double
akSpeed k = case k of
    Knight      -> 3.0
    FastKnight  -> 5.0

bboxOf :: BombKind -> Rect
bboxOf k = case k of
  Cow     -> fromDim 30   10
  Anchor  -> fromDim 15   15
  Vase    -> fromDim  5   20

-- TODO bbox doesn't match gfx yet
bboxOfA :: AttackerKind -> Rect
bboxOfA ak = fromDim 20   30

fromDim w h = Rect (-w/2, -h/2) (w/2, h/2)
