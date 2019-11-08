module Main where

import SlepysParser
import SlepysLexer
import SlepysPrettifier(prettify)
import SlepysSemantic
import Common

import Data.IORef
import System.Environment(getArgs)
import System.Exit
import System.Directory(doesFileExist)
import System.IO
import System.IO.Unsafe(unsafePerformIO)
import System.FilePath(replaceExtension)
import Control.Monad(unless)

exitCode :: IORef Int
{-# NOINLINE exitCode #-}
exitCode = unsafePerformIO (newIORef 0)

main :: IO ()
main = do
     args <- getArgs
     if "--help" `elem` args || "-help" `elem` args || "-h" `elem` args then do
        printHelp
        exitSuccess
     else if not $ null args then do
        sequence_ $ foldr (\f a -> if length a == 1 then processFile f : a
                                                    else processFile f : putStr "\n" : a
                          ) [pure ()] args
        eCode <- readIORef exitCode
        putStrLn $ "\nExit code: " ++ show eCode
        if eCode == 0 then
           exitSuccess
        else
           exitWith $ ExitFailure eCode
      else 
         putStrLn "Use slepys-formater[.exe] {-h | -help | --help} to show help"

processFile :: FilePath -> IO ()
processFile fileN = do
                    putStrLn fileN
                    fileExists <- doesFileExist fileN
                    if fileExists then do
                       h <- openFile fileN ReadMode 
                       buf <- hGetContents h
                       ast <- createAst buf
                       hClose h
                       unless (null ast) $ do
                          outputPretty fileN ast
                     --   abstract syntax tree debug output
                     --   print ast 
                          let sem = semanticAnalysis ast -- semantic analysis output
                          if not $ "Error" `isContainedIn` sem then do
                             defaultIndentOut "Semantic Analysis: OK"
                             putStrLn sem
                          else do
                             writeIORef exitCode 40
                             defaultIndentOut "Semantic Analysis: ERROR"
                             putStrLn sem
                    else do
                       defaultIndentOut "ERROR: does not appear to be existing file"
                       writeIORef exitCode 10
                       return ()

outputPretty :: FilePath -> Slepys -> IO ()
outputPretty origFileN ast = do
                             let newFileN = replaceExtension origFileN "pretty"
                             defaultIndentOut $ "Pretty file created: " ++ newFileN
                             h <- openFile newFileN WriteMode
                             hPutStr h $ prettify ast
                             hClose h
         


createAst :: String -> IO Slepys
createAst buf = case tokens buf of
                     Right tok -> do
                                  defaultIndentOut "Lexical Analysis: OK"
                              --  lexical analysis debug output
                              --  putStrLn (intercalate "\n" (map show tok))
                                  parsing tok
                     Left msg -> do 
                                 defaultIndentOut $ "Lexical Analysis: ERROR - " ++ msg
                                 writeIORef exitCode 20
                                 return []
                    
parsing :: [Token] -> IO Slepys
parsing ts = case parseTokens ts of
                  Right ast -> do
                               defaultIndentOut "Parsing: OK"
                               return ast
                  Left msg -> do
                              defaultIndentOut $ "Parsing: ERROR - " ++ msg
                              writeIORef exitCode 30
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
    where file = "slepys-formater[.exe]"
            