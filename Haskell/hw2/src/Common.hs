module Common where

import Data.List(tails, isPrefixOf, findIndex)

type Indent = Int

defaultIndent :: Int
defaultIndent = 3

indentList :: Indent -> [String] -> [String]
indentList indent = map (\x -> replicate indent ' ' ++ x)

makeIndent :: Indent -> String
makeIndent n = replicate n ' '

indentedPrint :: Indent -> String -> String
indentedPrint i s = makeIndent i ++ s

surround :: Char -> String -> String
surround c s | c == '('  = '(' : s ++ ")"
             | otherwise = c : s ++ [c]
 

deleteSurrounding :: String -> String
deleteSurrounding = init . tail

defaultIndentOut :: String -> IO ()
defaultIndentOut s = putStrLn $ makeIndent defaultIndent ++ s


isContainedIn :: (Eq a) => [a] -> [a] -> Bool
isContainedIn search str = case findIndex (isPrefixOf search) (tails str) of
                            Just _ -> True
                            _ -> False