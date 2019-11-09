module SlepysLexer(Lexem(..)
                    , LineNum
                    , Indent
                    , Token
                 , tokens
) where
import StringParser
import Data.List(elemIndex)

type LineNum = Int
type Indent = Int
type Token = (LineNum, Lexem)

data Lexem = Plus
           | Minus
           | Asterisks
           | Slash
           | Assign
           | Comma
           | Colon
           | LPar
           | RPar
           | Def
           | If
           | Else
           | While
           | Eq
           | Lt
           | Le
           | Gt
           | Ge
           | Semicolon
           | Pass
           | IndentIn
           | IndentOut
           | Identifier String
           | Val Integer
           | Str String
           | Whitespace Int
           | Newline Int
    deriving (Eq, Show)

insideString :: StringParser Char
insideString = do
              c <- item
              if c == '"' then
                empty
              else if c == '\\' then 
                item
              else
                return c


quotedString :: StringParser String
quotedString = do
               char '"'
               str <- many insideString
               char '"'
               return str

ident :: StringParser String
ident = (:) <$> identFirstLetter <*> many identNextLetter

identFirstLetter :: StringParser Char
identFirstLetter = lowerCase <|> char '_'

identNextLetter :: StringParser Char
identNextLetter = letter <|> char '_' <|> digit 

plus = char '+' >> return Plus
minus = char '-' >> return Minus
mult = char '*' >> return Asterisks
division = char '/' >> return Slash
assign = char '=' >> return Assign
comma = char ',' >> return Comma
colon = char ':' >> return Colon
lPar = char '(' >> return LPar
rPar = char ')' >> return RPar
def = string "def" >> return Def
if_ = string "if" >> return If
else_ = string "else" >> return Else
while = string "while" >> return While
eq = string "==" >> return Eq
lt = string "<" >> return Lt
le = string "<=" >> return Le
gt = string ">" >> return Gt
ge = string ">=" >> return Ge
semicolon = char ';' >> return Semicolon
pass = string "pass" >> return Pass
identifier = ident >>= \n -> return $ Identifier n
integer = int >>= \n -> return $ Val n
str = quotedString >>= \s -> return $ Str s
whitespace = spaces >>= \n -> return $ Whitespace n
newline = eols >>= \n -> return $ Newline n

lexem :: StringParser Lexem
lexem = plus
    <|> minus
    <|> mult
    <|> division
    <|> eq
    <|> assign
    <|> comma
    <|> colon
    <|> lPar
    <|> rPar
    <|> def
    <|> if_
    <|> else_
    <|> while
    <|> lt
    <|> le
    <|> gt
    <|> ge
    <|> semicolon
    <|> pass
    <|> identifier
    <|> integer
    <|> str
    <|> whitespace
    <|> newline

lexems :: StringParser [Lexem]
lexems = many lexem

tokens :: String -> Either String [Token]
tokens inp = case lex of
              [(tok, "")] -> addCtx tok [] 1 [0]
              [] -> Left "Undefined error" 
              [(_, xs)] -> Left $ "Undefined lexeme " ++ [head xs]
  where lex = parse lexems inp


addCtx :: [Lexem] -> [Token] -> LineNum -> [Indent] -> Either String [Token]
addCtx [] acc l iS = Right $ reverse $ replicate (length iS - 1) (l,IndentOut) ++ (l, Newline 1) : acc

addCtx [Whitespace _] acc l iS = addCtx [] acc l iS
addCtx [Newline n] acc l iS = addCtx [] ((l, Newline n) : acc) (l+n) iS
addCtx [tok] acc l iS = addCtx [] ((l, tok) : acc) l iS

addCtx (Newline n : Whitespace m : ts) acc l i | m == head i = addCtx ts ((l, Newline n) : acc) (l+n) i
                                                   | m > head i = addCtx ts ((l+n, IndentIn) : (l, Newline n) : acc) (l+n) (m:i)
                                                   | m < head i = case m `elemIndex` i of
                                                                      Just k -> addCtx ts (replicate k (l+n, IndentOut) ++ (l, Newline n ) : acc) (l+n) (drop k i)
                                                                      Nothing -> Left $ "On line " ++ show (l+n) ++ " incorrect attempt to IndentOut"

addCtx (Whitespace m : ts) acc l i = addCtx ts acc l i
addCtx (Newline n : ts) acc l i | i == [0]    = addCtx ts ((l, Newline n) : acc) (l+n) [0]
                                | otherwise = addCtx ts (replicate (length i -1) (l+n, IndentOut) ++ (l, Newline n) : acc) (l+n) [0]

addCtx (t : ts) acc l i = addCtx ts ((l,t) : acc) l i 
