module Types
  ( Pos
  , Board
  , ScreenState(..)
  , Dir(..)
  ) where

import qualified Data.Set as Set

type Pos = (Int, Int)

type Board = [Pos]

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
    , cells :: Set.Set Pos
    -- board width
    , width :: Int
    -- board height
    , height :: Int
    }

-- Dir = Direction
-- L = Left, ..., D = Down
-- used for moving cursor in move function in module Main
data Dir
  = L
  | R
  | U
  | D
  deriving (Eq)
