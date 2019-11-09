module SymbolTable(getSymbolLineUnchecked
                 , isDefined
                 , isDefinedInCurScope
                 , addSymbol
                 , newScope
                 , endOfScope
                 , SymbolTable
                 , nullSymblTbl) where

import SlepysParser
import Data.List(findIndex)

data Symbol = Ident Id | ScopeIn
    deriving Eq 

type SymbolTable = [Symbol]

nullSymblTbl :: SymbolTable
nullSymblTbl = [Ident (0, "print"), Ident (0, "read")]

getSymbolLineUnchecked :: SymbolTable -> String -> LineNum
getSymbolLineUnchecked tbl n = case getSymbol tbl n of
                                    Just (Ident (l, v)) -> l

isDefined :: SymbolTable -> String -> Bool
isDefined tbl n = case getSymbol tbl n of 
                    (Just _) -> True
                    _ -> False

isDefinedInCurScope :: SymbolTable -> String -> Bool
isDefinedInCurScope = isDefined . takeWhile (/=ScopeIn)

addSymbol :: SymbolTable -> Id -> SymbolTable
addSymbol tbl n = Ident n : tbl

newScope :: SymbolTable -> SymbolTable
newScope s = ScopeIn : s

endOfScope :: SymbolTable -> SymbolTable
endOfScope = tail . dropWhile (/=ScopeIn)

getSymbol :: SymbolTable -> String -> Maybe Symbol
getSymbol tbl n = case findIndex (idNameMatches n) tbl of 
                    (Just i) -> Just (tbl!!i)
                    Nothing -> Nothing
    where
        idNameMatches :: String -> Symbol -> Bool
        idNameMatches n (Ident (_, v)) = n == v
        idNameMatches n _ = False