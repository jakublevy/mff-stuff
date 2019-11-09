module Parser(Parser(parse), makeParser, (<|>), empty, some, many) where

import Control.Applicative
import Control.Monad.Fail
import Control.Monad((>=>))

newtype Parser s a = P { parse :: s -> [(a, s)] }

makeParser :: (s -> [(a, s)]) -> Parser s a
makeParser = P

instance Functor (Parser s) where
 -- fmap :: (a -> b) -> Parser s a -> Parser s b
    fmap f (P p) = P $ p >=> \(x, out) -> return (f x, out)

instance Applicative (Parser s) where
 -- pure :: a -> Parser s a 
    pure x = P $ \out -> [(x, out)]

 -- (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    P pf <*> p = P $ pf >=> \(f, inp2) -> parse (fmap f p) inp2

instance Monad (Parser s) where
 -- return = pure
 
 -- (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    P p >>= f = P $ p >=> \(x, inp2) -> parse (f x) inp2

instance Alternative (Parser s) where
 -- empty :: Parser s a
    empty = P $ const []

 -- (<|>) :: Parser s a -> Parser s a -> Parser s a
    P p1 <|> P p2 = P $ \inp -> case p1 inp of 
                                    [(x, out)] -> [(x, out)]
                                    [] -> p2 inp

 -- many :: Parser s a -> Parser s [a]
    many p = some p <|> pure []

 -- some :: Parser s a -> Parser s [a]
    some p = (:) <$> p <*> many p

instance MonadFail (Parser s) where
    fail msg = empty