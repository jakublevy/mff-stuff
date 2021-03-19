module Game
  ( nextGen
  , isAlive
  ) where

import Data.List
import qualified Data.Set as Set
import Types

-- inspired by the book Programming in Haskell 2nd edition
isAlive :: ScreenState -> Pos -> Bool
isAlive s pos = pos `Set.member` cells s

isEmpty :: ScreenState -> Pos -> Bool
isEmpty s pos = not $ isAlive s pos

-- external cell positions have neighbours on the other board side
-- using numbers mod width, mod height
wrap :: ScreenState -> Pos -> Pos
wrap s (x, y) = (x `mod` width s, y `mod` height s)

neighbs :: ScreenState -> Pos -> Board
neighbs s (x, y) =
  wrap s <$>
  [ (x - 1, y)
  , (x - 1, y - 1)
  , (x, y - 1)
  , (x + 1, y - 1)
  , (x + 1, y)
  , (x + 1, y + 1)
  , (x, y + 1)
  , (x - 1, y + 1)
  ]

liveNeighbsCount :: ScreenState -> Pos -> Int
liveNeighbsCount s pos = length [n | n <- neighbs s pos, isAlive s n]

survivors :: ScreenState -> Board
survivors s =
  [e | e <- Set.toList $ cells s, liveNeighbsCount s e `elem` [2, 3]]

births :: ScreenState -> Board
births s =
  [ e
  | e <- nub $ concatMap (neighbs s) $ Set.toList $ cells s
  , isEmpty s e
  , liveNeighbsCount s e == 3
  ]

nextGen :: ScreenState -> ScreenState
nextGen s = s {cells = Set.fromList $ survivors s ++ births s}
