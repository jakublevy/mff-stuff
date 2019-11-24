module MainBrick(mainBrick) where

import Brick
import qualified Brick.Types as T
import Brick.Util (bg, fg, on)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center)
import Control.Monad (void)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes as Color
import Network.Socket
import Control.Monad.IO.Class (liftIO)
import qualified Control.Exception as E

import qualified Data.Set as Set
import Game
import GameTypes

{- this is for creating the initial state -}
initState :: Handle -> ScreenState
initState h = ScreenState
                        { curPos = (0, 0)
                        , curHighlighted = True
                        , cells = Set.empty
                        , width = 20
                        , height = 20
                        , handle = h
                        }

putWriter :: Writer -> ScreenState -> ScreenState
putWriter w s = s { writer = w }

sendMsg :: ScreenState -> String -> IO ()
sendMsg s = write (writer s) (handle s)

quitBricks :: ScreenState -> EventM n2 (Next ScreenState)
quitBricks s = liftIO (sendMsg s "quit") >> halt s

{- This function converts the state to a list of widget layers to be drawn,
 - we only draw a single layer. -}
renderApp :: ScreenState -> [Widget n]
renderApp s = [center (title <=> mainLayer s)]

{- Operator <=> combines widgets vertically (as seen above),
 - operator <+> combines them horizontally. -}
-- mainLayer s = foldr1 (<+>) $ map (letter s) [0 .. 10]
mainLayer :: ScreenState -> Widget n
mainLayer s =
  border $
  foldr1
    (<+>)
    [ line2widget s [(x, y) | y <- [0 .. height s - 1]]
    | x <- [0 .. width s - 1]
    ]

line2widget :: ScreenState -> [Pos] -> Widget n
line2widget s xs = foldr1 (<=>) $ map (pos2widget s) xs

pos2widget :: ScreenState -> Pos -> Widget n
pos2widget s pos
  | Game.isAlive s pos =
    if curHighlighted s && pos == curPos s
      then withAttr cursorHl $ str "X"
      else str "X"
  | otherwise =
    if curHighlighted s && pos == curPos s
      then withAttr cursorHl $ str " "
      else str " "

title :: Widget n
title = border $ str "Game of Life"

cursorHl :: AttrName
cursorHl = attrName "cursorHl"

{- 
 - Event handlers that modify the state. They should in fact return a monad,
 - but that is vastly simplified by the helper functions:
 -
 - halt newstate   -- halts the app
 - continue newstate    -- continues executing the app with the supplied state
 -
 - The record update syntax is used as such:
 - originalItem{modifiedField = new value}
 -}
handleEvent :: ScreenState -> BrickEvent n1 e -> EventM n2 (Next ScreenState)
handleEvent s (T.VtyEvent (V.EvKey k [])) =
  case k of
    V.KEsc -> quitBricks s
    V.KLeft -> move L s
    V.KRight -> move R s
    V.KUp -> move U s
    V.KDown -> move D s
    -- V.KChar 'r' -> 
    V.KChar 'q' -> quitBricks s
    V.KChar 'h' -> move L s
    V.KChar 'j' -> move D s
    V.KChar 'k' -> move U s
    V.KChar 'l' -> move R s
    V.KChar 'v' -> continue $ s {curHighlighted = not (curHighlighted s)}
    V.KChar 'x' -> --TODO: send flip to server
                   do
                   let pos = curPos s
                   liftIO . sendMsg s $ "flip " ++ show (fst pos) ++ " " ++ show (snd pos)
                   continue s
      -- continue $
      -- if curPos s `elem` cells s
      --   then s {cells = curPos s `Set.delete` cells s}
      --   else s {cells = curPos s `Set.insert` cells s}
    V.KChar ' ' -> liftIO (sendMsg s "step") >> continue s
                 -- continue $ Game.nextGen s --TODO: change
    _ -> continue s --ignore all other keys
handleEvent s _ = continue s --also ignore all other events

move :: Dir -> ScreenState -> EventM n (Next ScreenState)
move d s
  | d == L = continue $ s {curPos = (max 0 (x - 1), y)}
  | d == R = continue $ s {curPos = (min (width s - 1) (x + 1), y)}
  | d == U = continue $ s {curPos = (x, max 0 (y - 1))}
  | d == D = continue $ s {curPos = (x, min (height s - 1) (y + 1))}
  where
    x = fst $ curPos s
    y = snd $ curPos s

{- This constructs the Brick app from the available functions -}
app :: App ScreenState e ()
app =
  App
    { appDraw = renderApp
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap =
        const $ attrMap V.defAttr [(cursorHl, Color.black `on` Color.cyan)]
    }

{- main just starts Brick main and discards (using void) the final state -}
mainBrick :: Socket -> IO ()
mainBrick sck = do
       h <- socketToHandle sck WriteMode
       hSetBuffering h (BlockBuffering Nothing)
       E.bracket
          initWriter
          (\w -> hClose h >> stop w)
          (\w -> void $ defaultMain app (putWriter w (initState h)))