module Villain.Logic -- TODO rename to *.Data, migrate lens back
    ( Point
    , BombKind(..)
    , Bomb(..)
    , Peg(..)
    , Hanger(..)
    , AttackerKind(..)
    , Attacker(..)
    , Level(..)

    , Rect(..)
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
    _falls  :: [Bomb],   -- TODO rename _bombs
    _aks    :: [Attacker]}


