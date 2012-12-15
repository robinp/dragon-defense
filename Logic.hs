module Villain.Logic
    ( Point
    , BombKind(..)
    , Bomb(..)
    , Peg(..)
    , Hanger(..)
    , AttackerKind(..)
    , Attacker(..)
    , Level(..)

    , Rect(..)
    , bboxOf
) where

data Cfg = Cfg

type Point = (Double, Double)

data Rect = Rect {
    _tl :: Point,
    _br :: Point}

data BombKind = Cow | Anchor | Vase

data Bomb = Bomb {
    _kindB  :: BombKind,
    _posB   :: Point,
    _speedB :: Double}

data Peg = Peg {
    _posP   :: Point}

data Hanger = Hanger {
    _pegH   :: Peg,
    _bombH  :: Bomb}

data AttackerKind = Knight | FastKnight

data Attacker = Attacker {
    _kindA  :: AttackerKind,
    _posA   :: Point}    

data Level = Level {
    _hangs  :: [Hanger],
    _falls  :: [Bomb],    
    _attackers  :: [Attacker]}

-- * Functions

akSpeed ak = case ak of
    Knight      -> 10.0
    FastKnight  -> 15.0

bboxOf :: BombKind -> Rect
bboxOf k = case k of
  Cow     -> fromDim 30   10
  Anchor  -> fromDim 15   15
  Vase    -> fromDim  5   20
  where
    fromDim w h = Rect (-w/2, -h/2) (w/2, h/2)
