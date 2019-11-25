{-# LANGUAGE TemplateHaskell #-}

module GameTypes
  ( Pos
  , ScreenState(..)
  , Dir(..)
  , defaultWidth
  , defaultHeight
  , module Writer
  , module Reader
  , module Lens.Micro
  , module Lens.Micro.TH
  , module Control.Concurrent.STM.TVar
  , curPos
  , curHighlighted
  , cells
  , width
  , height
  , writer
  , reader
  , speedVar
  , speed
  , curPosX
  , curPosY
  ) where

import Control.Concurrent.STM.TVar (TVar)
import Data.Set
import Lens.Micro
import Lens.Micro.TH

import Reader
import Writer

type Pos = (Int, Int)

defaultWidth :: Int
defaultWidth = 20

defaultHeight :: Int
defaultHeight = 20

{- 
 - ScreenState is our datatype for storing the state of the screen. The record
 - syntax is slightly more similar to the C-style structures; the "filed names"
 - are in fact accessor functions with types as such:
 - curPos :: ScreenState -> Int
 -}
data ScreenState =
  ScreenState
    -- (0,0) top left
    -- (width-1, height-1) bottom right
    { _curPos :: Pos
    -- key 'v' toggles highlighting
    , _curHighlighted :: Bool
    -- position of living cells
    , _cells :: Set Pos
    -- board width
    , _width :: Int
    -- board height
    , _height :: Int
    -- network writer
    , _writer :: Writer
    -- network reader
    , _reader :: Reader
    -- update speed TVar
    , _speedVar :: TVar Int
    -- update speed 
    , _speed :: Int
    }

makeLenses ''ScreenState

curPosX :: Functor f => (Int -> f Int) -> ScreenState -> f ScreenState
curPosX = curPos . _1

curPosY :: Functor f => (Int -> f Int) -> ScreenState -> f ScreenState
curPosY = curPos . _2

-- Dir = Direction
-- L = Left, ..., D = Down
-- used for moving cursor in move function in module Main
data Dir
  = L
  | R
  | U
  | D
  deriving (Eq)
