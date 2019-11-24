module GameTypes
  ( Pos
  , ScreenState(..)
  , Dir(..)
  , defaultWidth
  , defaultHeight
  , module Writer
  , module Reader
  ) where

import Data.Set
import Writer
import Reader

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
    { curPos :: Pos
    -- key 'v' toggles highlighting
    , curHighlighted :: Bool
    -- position of living cells
    , cells :: Set Pos
    -- board width
    , width :: Int
    -- board height
    , height :: Int
    -- network writer
    , writer :: Writer
    -- network reader
    , reader :: Reader
    }

-- Dir = Direction
-- L = Left, ..., D = Down
-- used for moving cursor in move function in module Main
data Dir
  = L
  | R
  | U
  | D
  deriving Eq
