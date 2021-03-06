{-# LANGUAGE TemplateHaskell #-}

module Villain.Game (main) where

import GHC.Float

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Control.Error

import Data.List (find, partition)
import Data.Maybe (isJust, fromJust)

import Codec.Picture
import Codec.BMP
import Graphics.Gloss hiding (Point)
import qualified Graphics.Gloss.Interface.Pure.Game as GG

import RWSExtras

import Villain.Logic
import Villain.Logic.Lens


data Pics = Pics {
  _akPics       :: [(AttackerKind, Picture)],
  _bombPics     :: [(BombKind, Picture)], 
  _pegPic       :: Picture,
  _dragonPic    :: Picture,
  _bgPic        :: Picture
}
--
makeLenses ''Pics

drawGame :: Pics -> Game -> Picture
drawGame pics g = Pictures [lvl, dialog, mouse']
  where lvl = drawLevel pics $ g^.level
        dialog = case g^.phase of
                      InGame    -> Blank
                      Lost      -> dialogRect red
                      Success   -> dialogRect green
        dialogRect col = Color col $ drawRect $ Rect (-50, 30) (50, -30)
        mouse' = drawMouse pics (g^.mouse)

drawMouse pics m = 
  let (_, y) = biD2F m
  in  Translate dragonX (max (double2Float dragonMinY) y) $ pics^.dragonPic

drawLevel pics lvl = Pictures $ [drawBg] ++ drawHangs ++ drawAks ++ drawBombs ++ [drawNotify]
  where drawHangs = drawHang pics <$> lvl^.hangs
        drawAks = drawAk pics <$> lvl^.aks
        drawBombs = drawBomb pics <$> lvl^.falls
        drawBg = pics^.bgPic

drawNotify = Translate (-250) 200 $ Color black $ Scale 0.15 0.15 $ Text "Click to fire"

drawBomb pics b =
  let (x, y) = bombXY b
  in  Translate x y $ lookup' (b^.kindB) (pics^.bombPics)  -- $ Color red $ drawRect $ bboxOf (b^.kindB)

bombXY = biD2F . view posB

drawHang pics h = 
    let (px, py) = biD2F $ h^.pegH.posP
        (bx, by) = bombXY $ h^.bombH
        peg   = Translate px py $ pics^.pegPic --Color red $ ThickCircle 10 3
        rope  = Color red $ Line [(px, py), (bx, by)]
        bomb  = drawBomb pics $ h^.bombH
    in
      Pictures [rope, peg, bomb]


biD2F = both %~ double2Float
biF2D = both %~ float2Double

drawAk pics ak =
    let (x, y) = biD2F $ ak^.posA
    in  Translate x y $ lookup' (ak^.kindA) (pics^.akPics)

lookup' :: (Eq a) => a -> [(a, b)] -> b
lookup' a = snd . fromJust . find (views _1 (a ==))

drawRect :: Rect -> Picture
drawRect (Rect tl br) = Polygon $ mapped.both %~ double2Float $ [tl, yx tl br, br, xy tl br, tl]
  where xy (x, _) (_, y) = (x, y)
        yx (_, y) (x, _) = (x, y)

initLevel = Level hs [] aks
  where hs  = [
          mkHang Cow 100 hangBase 100,
          mkHang Anvil (-100) (hangBase-50) 100 ]
        aks = [
          mkAk Knight (-400),
          mkAk FastKnight (-550) ]

mkHang bk x y h = Hanger (Peg (x, y)) (Bomb bk (x, y-h) 0) 
mkAk ak x = Attacker ak (x, floorLine)

initGame = Game initLevel InGame (0, 0)

--
-- TODO global input state, use Reader
--
fps = 20
towerX = 250
dragonX = 300
dragonMinY = floorLine + 150
floorLine = -220 :: Double
hangBase = 100
gravity = 0.3

bombImageName bk = case bk of
  Cow   -> "cow.bmp"
  Anvil -> "anvil.bmp"
  Goat  -> "goat.bmp"
  Plant -> "plant.bmp"
  Piano -> "piano.bmp"

akImageName ak = case ak of
  Knight      -> "knight.bmp"
  FastKnight  -> "fastknight.bmp"

main :: IO ()
main = do
  peg         <- loadBMP "peg.bmp"
  dragon      <- loadBMP "dragon.bmp"
  bg          <- loadBMP "bg.bmp"
  --
  let bombKinds = [Cow, Anvil, Goat, Plant, Piano]
  bombs <- mapM loadBMP $ bombImageName <$> bombKinds
  let bombPics' = zip bombKinds bombs
  --
  let aKinds = [Knight, FastKnight]
  aks <- mapM loadBMP $ akImageName <$> aKinds
  let akPics' = zip aKinds aks
  --
  let pics = Pics akPics' bombPics' peg dragon bg
  let draw = drawGame pics
  play (InWindow "Liter" (800, 600) (50, 50)) white fps initGame draw gameInput gameUpdate

gameInput ev game = 
  game & case game^.phase of
              InGame    -> ingameInput ev
              otherwise -> id

ingameInput :: GG.Event -> Game -> Game
ingameInput ev = execState $ do
  maybe (return ()) ((mouse .=) . biF2D) (updateFromEv ev)
  when (fireFromEv ev) doFire
  where
    updateFromEv ev = 
      case ev of GG.EventMotion pos -> Just pos
                 otherwise          -> Nothing

    fireFromEv ev =
      case ev of GG.EventKey (GG.MouseButton _) GG.Down _ _ -> True
                 otherwise                                  -> False
    
    doFire = do
      dragonPos <- use $ mouse._2
      level %= execState (detachAll $ isHangerCutAt dragonPos)

isHangerCutAt y = do
  pegY <- view $ pegH.posP._2
  bombY <- view $ bombH.posB._2
  return $ bombY <= y && y <= pegY

gameUpdate dt game =
  game & case game^.phase of
              InGame    -> execState ingameUpdate
              otherwise -> id

ingameUpdate :: State Game ()
ingameUpdate = do
  level %= (bombAks . moveBombs . moveAks)
  levelFailed <- use $ level.aks.to (any reachedTower)
  levelWon <- use $ level.aks.to null
  when levelFailed $ phase .= Lost
  when levelWon $ phase .= Success

reachedTower :: Attacker -> Bool
reachedTower = views (posA._1) (towerX <)

-- | Detaches all bombs from the passing hangers
detachAll :: (Hanger -> Bool) -> State Level ()
detachAll p =
  detachFirst p >>= (flip when $ detachAll p)

-- | Tries to detach a bomb and tells if succeeded
detachFirst :: (Hanger -> Bool) -> State Level Bool
detachFirst p = do
  mayH <- withSubSt hangs $ popHanger p
  falls %= (maybe [] (return . _bombH) mayH ++)
  return $ isJust mayH

popHanger :: (Hanger -> Bool) -> State [Hanger] (Maybe Hanger)
popHanger p = do
  mayHanger <- gets $ find p
  modify $ removeFst p
  return mayHanger
  where
    removeFst p xs = 
      let (as, bs) = break p xs
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
  let boundB = bboxOf (b^.kindB)
      boundA = bboxOfA (ak^.kindA)
      rectB = offRect (b^.posB) boundB
      rectA = offRect (ak^.posA) boundA
  in
    intersects rectA rectB

intersects r1 r2 =
  not (r2^.tl._1 > r1^.br._1 ||
       r2^.br._1 < r1^.tl._1 ||
       r2^.tl._2 > r1^.br._2 ||
       r2^.br._2 < r1^.tl._2)

offRect :: Point -> Rect -> Rect
offRect (dx,dy) (Rect (x1,y1) (x2,y2)) = Rect (x1+dx,y1+dy) (x2+dx,y2+dy)

moveBombs :: Level -> Level
moveBombs = disappearBombs . fallBombs
  where disappearBombs = falls %~ filter (views (posB._2) (floorLine <))
                               -- filter (\b -> b^.posB._2 > floorLine) -- is this simpler?
        fallBombs = falls.mapped %~ execState fallBomb
        fallBomb = do
          spd <- speedB <+= gravity
          posB._2 -= spd

akSpeed :: AttackerKind -> Double
akSpeed k = case k of
    Knight      -> 3.0
    FastKnight  -> 5.0

bboxOf :: BombKind -> Rect
bboxOf k = case k of
  Cow     -> fromDim  105  65
  Anvil   -> fromDim   50  34  
  Goat    -> fromDim   72  61
  Plant   -> fromDim   39  46
  Piano   -> fromDim  160 145  

bboxOfA :: AttackerKind -> Rect
bboxOfA ak = case ak of
  Knight      -> fromDim  64  96
  FastKnight  -> fromDim  67 106

fromDim w h = Rect (-w/2, -h/2) (w/2, h/2)
