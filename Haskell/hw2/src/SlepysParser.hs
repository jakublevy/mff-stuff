module SlepysParser(parseTokens
                  , Slepys
                  , Expr(..)
                  , Id
                  , LineNum
                  , Method(..)
                  , MethodHeader(..)
                  , Statement(..)
                  ) where

import TokenParser

type Id = (LineNum, String)

data Expr = IntConst Integer
          | StringConst String
          | Id Id
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | Div Expr Expr
          | Call Id [[Expr]]
          | Equal Expr Expr
          | Less Expr Expr
          | Greater Expr Expr
          | LessOrEqual Expr Expr
          | GreaterOrEqual Expr Expr
    deriving Show --debug purpose

data Method = Method { header :: MethodHeader
                     , body :: [Statement]
                     }
            deriving Show --debug purpose

data MethodHeader = MethodHeader { identifier :: Id 
                                 , parameters :: [Id]
                                 }
                    deriving Show --debug purpose

data Statement = Expr Expr
               | While Expr [Statement]
               | If Expr [Statement] [Statement]
               | MethodDef Method
               | Assignment Id Expr
               | Skip 
            deriving Show --debug purpose

type Slepys = [Statement]

formalParameters :: TokenParser [Id]
formalParameters = do
                   (i, Identifier n) <- token
                   rest <- many $ do
                                  Comma <- lexem
                                  (j, Identifier m) <- token
                                  return (j, m)

                   return $ (i, n) : rest 
               <|> pure []

methodHeader :: TokenParser MethodHeader
methodHeader = do
               Def <- lexem
               (i, Identifier mName) <- token
               LPar <- lexem
               params <- formalParameters
               RPar <- lexem
               Colon <- lexem
               return $ MethodHeader (i,mName) params

blockBody :: TokenParser [Statement]
blockBody = do
            IndentIn <- lexem
            stms <- some statement 
            IndentOut <- lexem
            return stms
        <|> stmtsSemicolonTerm


pass :: TokenParser Statement
pass = do
       Pass <- lexem
       return Skip

method :: TokenParser Statement
method = do
         h <- methodHeader
         b <- blockBody
         let m = Method { 
             header = h
           , body = b
         }
         return $ MethodDef m

factor :: TokenParser Expr
factor =     do
            Val n <- lexem
            return $ IntConst n
         <|> do
            Str s <- lexem
            return $ StringConst s
         <|> do
            (i, Identifier id) <- token
            xss <- some $ do
                   LPar <- lexem
                   args <- arguments 
                   RPar <- lexem
                   return args
            return $ Call (i, id) xss
         <|> do
            (i, Identifier id) <- token
            return $ Id (i, id)
         <|> do
            LPar <- lexem
            e <- expr
            RPar <- lexem
            return e

arguments :: TokenParser [Expr]
arguments =  do
             e <- expr
             es <- many $ do
                          Comma <- lexem
                          expr
             return $ e : es
         <|> pure []

createExpr :: Expr -> (Lexem, Expr) -> Expr
createExpr e1 (Gt, e2) = Greater e1 e2
createExpr e1 (Lt, e2) = Less e1 e2
createExpr e1 (Ge, e2) = GreaterOrEqual e1 e2
createExpr e1 (Le, e2) = LessOrEqual e1 e2
createExpr e1 (Eq, e2) = Equal e1 e2
createExpr e1 (Plus, e2) = Add e1 e2
createExpr e1 (Minus, e2) = Sub e1 e2
createExpr e1 (Asterisks, e2) = Mult e1 e2
createExpr e1 (Slash, e2) = Div e1 e2

functor :: TokenParser Expr
functor = do
          f <- factor
          funcOps <- many $ do
                            op <- binOp1
                            e <- factor
                            return (op, e)
          return $ foldl createExpr f funcOps

term :: TokenParser Expr
term = do
       f <- functor
       funcOps <- many $ do
                         op <- binOp2
                         e <- functor
                         return (op, e)
       return $ foldl createExpr f funcOps

expr :: TokenParser Expr
expr = do
       t <- term
       termOps <- many $ do
                         op <- relOp
                         t <- term
                         return (op, t)
       return $ foldl createExpr t termOps
        
statementLexem :: TokenParser Lexem
statementLexem = g <$> lexem
    where 
        g :: Lexem -> Lexem
        g l |   l == TokenParser.While
             || l == TokenParser.If    = l

isBooleanExpr :: Expr -> Bool
isBooleanExpr (Equal _ _) = True
isBooleanExpr (Less _ _) = True
isBooleanExpr (Greater _ _) = True
isBooleanExpr (LessOrEqual _ _) = True
isBooleanExpr (GreaterOrEqual _ _) = True
isBooleanExpr _ = False

booleanExpr :: TokenParser Expr
booleanExpr = do
              e <- expr
              if isBooleanExpr e then
                 return e
              else empty 

semicolon :: TokenParser Lexem
semicolon = do
            Semicolon <- statementEnd
            return Semicolon

stmtsSemicolonTerm :: TokenParser [Statement]
stmtsSemicolonTerm = do
                     t <- top
                     case t of
                        (_, Newline _) -> empty
                        _ -> do
                             s <- assignment <|> pass <|> do Expr <$> expr
                             end <- statementEnd
                             case end of 
                                Semicolon -> do
                                     many semicolon
                                     t <- top
                                     case t of
                                        (_, Newline _) -> return [s]
                                        _ -> do
                                             ss <- stmtsSemicolonTerm 
                                             return $ s : ss
                                (Newline _) -> return [s]

statement :: TokenParser Statement
statement =     do
                kw <- statementLexem
                be <- booleanExpr
                Colon <- lexem
                b <- blockBody
                if kw == TokenParser.If then
                    do
                        Else <- lexem
                        Colon <- lexem
                        SlepysParser.If be b <$> blockBody
                    <|>
                    do
                        return $ SlepysParser.If be b []
                else 
                        return $ SlepysParser.While be b

            <|> do
                s <- assignment <|> pass <|> do Expr <$> expr
                some statementEnd
                return s

            <|> method

statementEnd :: TokenParser Lexem
statementEnd = do
               n <- next
               case n of
                    (_, Semicolon) -> return Semicolon
                    (_, Newline n) -> return $ Newline n
                    _ -> empty
                

relOp :: TokenParser Lexem
relOp = do
        l <- lexem
        if    l == Gt
           || l == Lt
           || l == Ge
           || l == Le 
           || l == Eq then return l
                      else empty

binOp2 :: TokenParser Lexem
binOp2 = do
         l <- lexem
         if   l == Plus
           || l == Minus then return l
                         else empty

binOp1 :: TokenParser Lexem
binOp1 = do
         l <- lexem
         if   l == Asterisks
           || l == Slash    then return l
                            else empty

assignment :: TokenParser Statement
assignment = do
             (i, Identifier n) <- token
             Assign <- lexem
             Assignment (i, n) <$> expr

trailingNewline :: TokenParser Lexem
trailingNewline = do
                  (_, Newline n) <- next
                  return $ Newline n

slepys :: TokenParser Slepys
slepys = many statement >>= \prog -> many trailingNewline >> return prog

parseTokens :: [Token] -> Either String Slepys
parseTokens tok = case parse slepys tok of
                    [(stms, [])] -> Right stms
                    [] -> Left "Undefined parsing error occurred"
                    [(_, (erLine, erLex) : xs)] -> Left $ "Error on line " ++ show erLine ++ " near " ++ show erLex