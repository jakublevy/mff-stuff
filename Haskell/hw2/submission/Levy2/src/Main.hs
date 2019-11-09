-- I didn't use hindent because it destroys some of my code
-- Some of it does not compile, some of it does, but doesn't output what it is supposed to

module Main where

import SlepysParser
import SlepysLexer
import SlepysPrettifier(prettify)
import SlepysSemantic
import Common

import System.Environment(getArgs)
import System.Exit
import System.Directory(doesFileExist)
import System.IO
import System.FilePath(replaceExtension)
import Control.Monad(unless)
import Control.Monad.State

io :: (MonadTrans t, Monad m) => m a -> t m a
io = lift

setExitCode :: Int -> StateT Int IO ()
setExitCode = put

main :: IO ()
main = do
     args <- getArgs
     if "--help" `elem` args || "-help" `elem` args || "-h" `elem` args then do
        printHelp
        exitSuccess
     else if not $ null args then do
        eCode <- processFiles args
        putStrLn $ "\nExit code: " ++ show eCode
        if eCode == 0 then
           exitSuccess
        else
           exitWith $ ExitFailure eCode
      else 
         putStrLn "Use slepys-format[.exe] {-h | -help | --help} to show help"

processFiles :: [FilePath] -> IO Int
processFiles args = do
                    run <- sequenceA $ foldr (\f a -> if null a then runStateT (processFile f) 0 : a
                                                                else runStateT (processFile f) 0 : newline : a) [] args
                    return $ snd $ last run

newline :: IO ((), Int)
newline = ((), dummy) <$ putStr "\n"
   where dummy = 0

processFile :: FilePath -> StateT Int IO ()
processFile fileN = do
                    io $ putStrLn fileN
                    fileExists <- io $ doesFileExist fileN
                    if fileExists then do
                       h <- io $ openFile fileN ReadMode 
                       buf <- io $ hGetContents h
                       ast <- createAst buf
                       io $ hClose h
                       unless (null ast) $ do
                          io $ outputPretty fileN ast
                     --   abstract syntax tree debug output
                     --   io $ print ast 
                          let sem = semanticAnalysis ast -- semantic analysis output
                          if not $ "Error" `isContainedIn` sem then do
                             io $ defaultIndentOut "Semantic Analysis: OK"
                             io $ putStrLn sem
                          else do
                             setExitCode 40 
                             io $ defaultIndentOut "Semantic Analysis: ERROR"
                             io $ putStrLn sem
                    else do
                       io $ defaultIndentOut "ERROR: does not appear to be existing file"
                       setExitCode 10
                       return ()

outputPretty :: FilePath -> Slepys -> IO ()
outputPretty origFileN ast = do
                             let newFileN = replaceExtension origFileN "pretty"
                             defaultIndentOut $ "Pretty file created: " ++ newFileN
                             h <- openFile newFileN WriteMode
                             hPutStr h $ prettify ast
                             hClose h
         


createAst :: String -> StateT Int IO Slepys
createAst buf = case tokens buf of
                     Right tok -> do
                                  io $ defaultIndentOut "Lexical Analysis: OK"
                              --  lexical analysis debug output
                              --  io $ putStrLn (intercalate "\n" (map show tok))
                                  parsing tok
                     Left msg -> do 
                                 io $ defaultIndentOut $ "Lexical Analysis: ERROR - " ++ msg
                                 setExitCode 20
                                 return []
                    
parsing :: [Token] -> StateT Int IO Slepys
parsing ts = case parseTokens ts of
                  Right ast -> do
                               io $ defaultIndentOut "Parsing: OK"
                               return ast
                  Left msg -> do
                              io $ defaultIndentOut $ "Parsing: ERROR - " ++ msg
                              setExitCode 30
                              return []


printHelp :: IO ()
printHelp = putStrLn $ "Usage: " ++ file ++ " <FILE_PATH> [..n]"
                 ++ "\n       " ++  file ++ " {-h | -help | --help}"
                 ++ "\n\nThe following example creates (in case of success) two output files fact.pretty and fib.pretty"
                 ++ "\nExample: slepys[.exe] fact.slepys fib.slepys"
                 ++ "\n\nExit codes:  0 - Success"
                 ++ "\n            10 - Supplied argument does not point to an existing file"
                 ++ "\n            20 - Lexical Analysis failed"
                 ++ "\n            30 - Parsing failed"
                 ++ "\n            40 - Semantic Analysis found errors"
    where file = "slepys-format[.exe]"
            