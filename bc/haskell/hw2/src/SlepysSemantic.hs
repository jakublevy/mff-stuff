module SlepysSemantic(semanticAnalysis) where

import SymbolTable
import Control.Monad.State
import Common
import SlepysParser
import SlepysPrettifier
import Data.List(intercalate, findIndices, findIndex)

data Context = Ctx { symbols :: SymbolTable
                   , locationDesc :: [String]
                   }

type ErrorMsg = String

nullCtx :: Context 
nullCtx = Ctx {symbols = nullSymblTbl, locationDesc = []}

semanticAnalysis :: Slepys -> String
semanticAnalysis slepys = intercalate "\n" . indentList (2*defaultIndent) $ evalState (stmtsAnalysis slepys) nullCtx

addSymbl :: Id -> State Context ()
addSymbl id = do
              ctx <- get
              let newSymblTbl = addSymbol (symbols ctx) id
              put $ ctx { symbols = newSymblTbl }

addSymblHidingErr :: Id -> State Context [ErrorMsg]
addSymblHidingErr (l, n) = do
                    ctx <- get
                    let ll = getSymbolLineUnchecked (symbols ctx) n
                    let err = "Warning: On line " ++ show l ++ " hiding outer identifier " ++ surround '\'' n ++ " defined on line " ++ show ll
                    addSymbl (l, n)
                    addErrorCtx err

addParamsHidingChck :: [Id] -> State Context [ErrorMsg]
addParamsHidingChck [] = pure []
addParamsHidingChck ((l,n) : ids) = do
                                    ctx <- get
                                    let d = isDefined (symbols ctx) n
                                    if d then do
                                      err <- addSymblHidingErr (l, n)
                                      errs <- addParamsHidingChck ids
                                      return $ err ++ errs
                                    else do
                                      addSymbl (l, n)
                                      addParamsHidingChck ids

scopeIn :: State Context ()
scopeIn = do
          ctx <- get
          let newSymblTbl = newScope (symbols ctx) 
          put $ ctx { symbols = newSymblTbl }

scopeOut :: State Context ()
scopeOut = do
           ctx <- get
           let newSymblTbl = endOfScope (symbols ctx) 
           put $ ctx { symbols = newSymblTbl }

addErrorCtx :: String -> State Context [ErrorMsg] 
addErrorCtx err = do
                  ctx <- get
                  let errWCtx = intercalate "\n" $ err : indentList (3*defaultIndent) (locationDesc ctx)
                  return [errWCtx]

addNewCtx :: String -> State Context ()
addNewCtx s = do
              ctx <- get 
              put $ ctx { locationDesc = s : locationDesc ctx }

removeLastCtx :: State Context ()
removeLastCtx = do
                ctx <- get
                put $ ctx { locationDesc = tail $ locationDesc ctx}

stmtAnalysis :: Statement -> State Context [ErrorMsg] 
stmtAnalysis (Expr e) = do
                        addNewCtx $ "found in expression: " ++ prettifyOneStmt (Expr e)
                        exprErrs <- exprAnalysis e
                        removeLastCtx
                        return exprErrs

stmtAnalysis Skip = pure []
stmtAnalysis (Assignment (l, n) e) = do
                                     addNewCtx $ "found in assignment: " ++ prettifyOneStmt (Assignment (l,n) e)
                                     rhsErrs <- exprAnalysis e
                                     ctx <- get
                                     let d = isDefined (symbols ctx) n
                                     if not d then do
                                        addSymbl (l, n)
                                        removeLastCtx
                                        return rhsErrs
                                     else do
                                        let d2 = isDefinedInCurScope (symbols ctx) n 
                                        if not d2 then do
                                            let ll = getSymbolLineUnchecked (symbols ctx) n
                                            lhsErrs <- addSymblHidingErr (l, n)
                                            removeLastCtx
                                            return $ lhsErrs ++ rhsErrs
                                        else do
                                            removeLastCtx
                                            return rhsErrs

stmtAnalysis (MethodDef m) = do
                             headerErrs <- methodHeaderAnalysis (header m)
                             bodyErrs <- methodBodyAnalysis (parameters $ header m) (body m)
                             removeLastCtx
                             return $ headerErrs ++ bodyErrs

stmtAnalysis (While e stmts) = do
                               addNewCtx $ "found inside: " ++ surround '\'' ("while " ++ deleteSurrounding (prettifyOneStmt (Expr e)))
                               lhsErrs <- exprAnalysis e
                               bodyErrs <- blockBodyAnalysis stmts
                               removeLastCtx
                               return $ lhsErrs ++ bodyErrs

stmtAnalysis (If e stmts1 stmts2) = do
                              addNewCtx $ "found inside: " ++ surround '\'' ("if " ++ deleteSurrounding (prettifyOneStmt (Expr e)))
                              lhsErrs <- exprAnalysis e
                              bodyErrs <- blockBodyAnalysis stmts1
                              removeLastCtx
                              addNewCtx $ "found inside: " ++ surround '\'' "else"
                              elseBErrs <- blockBodyAnalysis stmts2
                              removeLastCtx
                              return $ lhsErrs ++ bodyErrs ++ elseBErrs

alreadyIn :: Id -> [Id] -> Bool
alreadyIn (l, n) xs = case idx of 
                          Just i -> True
                          _ -> False
    where idx = findIndex (\(_,n') -> n == n') xs

dupliciteIdentifiers :: [Id] -> [Id]
dupliciteIdentifiers xs = foldr (\(l,x) a -> let idxs = findIndices (\(l', y) -> x == y) xs
                                             in if length idxs > 1 then if (l,x) `alreadyIn` a then a
                                                                                               else (l,x):a
                                                                   else a) [] xs

duplErrors :: [Id] -> State Context [ErrorMsg]
duplErrors [] = pure []
duplErrors ((l, n) : ids) = do
                        let err = "Error: On line " ++ show l ++ " duplicite parameter " ++ surround '\'' n 
                        errsCtx <- addErrorCtx err
                        errs <- duplErrors ids
                        return $ errsCtx ++ errs
                                          

methodBodyAnalysis :: [Id] -> [Statement] -> State Context [ErrorMsg]
methodBodyAnalysis params stmts = do
                                  let dupl = dupliciteIdentifiers params
                                  duplErrs <- duplErrors dupl
                                  scopeIn
                                  hidingErrs <- addParamsHidingChck params
                                  bodyErrs <- stmtsAnalysis stmts
                                  scopeOut
                                  return $ duplErrs ++ hidingErrs ++ bodyErrs


blockBodyAnalysis :: [Statement] -> State Context [ErrorMsg]
blockBodyAnalysis stmts = scopeIn >> stmtsAnalysis stmts >>= \bodyErrs -> scopeOut >> return bodyErrs

stmtsAnalysis :: [Statement] -> State Context [ErrorMsg]
stmtsAnalysis = foldr (\s -> (<*>) ((++) <$> stmtAnalysis s)) (pure [])

methodHeaderAnalysis :: MethodHeader -> State Context [ErrorMsg]
methodHeaderAnalysis mh = do
                          ctx <- get
                          let (l,n) = identifier mh
                          let d = isDefinedInCurScope (symbols ctx) n
                          if d then do
                            let err = "Error: On line " ++ show l ++ " redefinition of identifier " ++ surround '\'' n
                            errs <- addErrorCtx err
                            addNewCtx $ "found in method: " ++ prettifyOneMh mh 
                            addSymbl (l, n)
                            return errs
                          else do
                            let d2 = isDefined (symbols ctx) n
                            if d2 then do
                                errs <- addSymblHidingErr (l, n)
                                addNewCtx $ "found inside method: " ++ prettifyOneMh mh 
                                return errs
                            else do
                                addSymbl (l, n)
                                addNewCtx $ "found inside method: " ++ prettifyOneMh mh 
                                return []


                             

exprAnalysis :: Expr -> State Context [ErrorMsg]
exprAnalysis (Id (l,n)) = do
                          ctx <- get
                          let b = isDefined (symbols ctx) n
                          if b then 
                            return []
                          else do
                            let errMsg = "Error: On line " ++ show l ++ " undefined identifier " ++ surround '\'' n
                            addErrorCtx errMsg

       
exprAnalysis (Add e1 e2) = exprsAnalysis[e1, e2]
exprAnalysis (Sub e1 e2) = exprsAnalysis[e1, e2]
exprAnalysis (Mult e1 e2) = exprsAnalysis[e1, e2]
exprAnalysis (Div e1 e2) = exprsAnalysis[e1, e2]
exprAnalysis (Equal e1 e2) = exprsAnalysis[e1, e2]
exprAnalysis (Less e1 e2) = exprsAnalysis[e1, e2]
exprAnalysis (Greater e1 e2) = exprsAnalysis[e1, e2]
exprAnalysis (LessOrEqual e1 e2) = exprsAnalysis[e1, e2]
exprAnalysis (GreaterOrEqual e1 e2) = exprsAnalysis[e1, e2]

exprAnalysis (Call id ess) = (++) <$> exprAnalysis (Id id) <*> exprsAnalysis (concat ess)
                    
-- IntConst case
-- StringConst case
exprAnalysis _ = pure []

exprsAnalysis :: [Expr] -> State Context [ErrorMsg]
exprsAnalysis = foldr (\e -> (<*>) ((++) <$> exprAnalysis e)) (pure [])

rTrim :: String -> String
rTrim = takeWhile (\x -> x /= ';' && x /= '{' &&  x /= ':')
  
prettifyOneStmt :: Statement -> String
prettifyOneStmt stmt = surround '\'' . rTrim $ evalState (prettifyStatement stmt) 0
   
prettifyOneMh :: MethodHeader -> String
prettifyOneMh mh = surround '\'' . rTrim $ evalState (prettifyMethodHeader mh) 0
   