module Writer
  ( Writer(DummyWriter)
  , initWriter
  , write
  ) where

import Control.Concurrent
import Control.Concurrent.MVar (MVar)
import System.IO (Handle, hFlush, hPutStr)

data Writer
  = Writer (MVar String)
  | DummyWriter

initWriter :: Handle -> IO (Writer, ThreadId)
initWriter h = do
  m <- newEmptyMVar
  let w = Writer m
  tId <- forkIO $ writer h w
  return (w, tId)

writer :: Handle -> Writer -> IO ()
writer h (Writer m) = loop
  where
    loop = do
      msg <- takeMVar m
      hPutStr h $ msg ++ "\r\n"
      hFlush h
      loop

write :: Writer -> String -> IO ()
write (Writer m) msg = do
  forkIO $ putMVar m msg
  return ()