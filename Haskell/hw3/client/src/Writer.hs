module Writer(Writer
           , WriteCmd(..)
           , initWriter
           , write
           , stop) where

import Control.Concurrent
import Control.Concurrent.MVar
import System.IO

newtype Writer = Writer (MVar WriteCmd)
data WriteCmd = Message Handle String | Stop (MVar ())

initWriter :: IO Writer
initWriter = do
             m <- newEmptyMVar
             let w = Writer m
             forkIO $ writer w
             return w

writer :: Writer -> IO ()
writer (Writer m) = loop
    where
        loop = do
               cmd <- takeMVar m
               case cmd of
                    Message h msg -> do
                                     hPutStr h $ msg ++ "\r\n"
                                     hFlush h
                                     loop
                    Stop s -> putMVar s ()


write :: Writer -> Handle -> String -> IO ()
write (Writer m) h msg = do 
                         forkIO $ putMVar m (Message h msg)
                         return ()

stop :: Writer -> IO ()
stop (Writer m) = do
                  s <- newEmptyMVar
                  putMVar m (Stop s)
                  takeMVar s