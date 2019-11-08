{-# LANGUAGE LambdaCase #-}

module TokenParser(TokenParser
                 , token
                 , lexem
                 , next
                 , top
                 , module SlepysLexer
                 , module Parser) where

import SlepysLexer
import Parser

type TokenParser = Parser [Token]

next :: TokenParser Token
next = makeParser $ \case
                     ((l,t) : xs) -> [((l, t), xs)]
                     [] -> []
                     
top :: TokenParser Token
top = makeParser $ \case 
                    ((l,t) : xs) -> [((l,t), (l,t) : xs)]
                    [] -> []

lexem :: TokenParser Lexem
lexem = snd <$> token

token :: TokenParser Token
token = do
        (line, lex) <- next
        case lex of 
            Newline n -> token
            _ -> return (line, lex)
        