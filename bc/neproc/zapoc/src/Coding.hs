module Coding(encode,decode) where

import Types
import String2occurrence
import Occurrence2tree
import Tree2assoc
import AssocString2code
import qualified Data.List as List

--encode "hello"
--returns ([0,1,0,0,1,1,1,1,1,0],[('e',[0,0]),('h',[0,1]),('o',[1,0]),('l',[1,1])])
encode :: String -> (Code, Assoc)
encode xs = (assocString2code tab xs, tab)
            where tab = tree2assoc $ occurrence2tree $ string2occurrence xs


--decode ([0,1,0,0,1,1,1,1,1,0],[('e',[0,0]),('h',[0,1]),('o',[1,0]),('l',[1,1])])
--returns "hello"
decode :: (Code, Assoc) -> String
decode ([], _) = ""
decode ((b : bs), tab) = decode' [b] bs
                        where 
                        decode' k ls = case findCharCode k tab of
                                                    Just (c,_) -> if null ls then
                                                                    c : [] --c is a valid character and we have run out of codes
                                                                  else 
                                                                    c : decode' [head ls] (tail ls) --we have found valid code and corresponding char
                                                                                                    --looking for a next valid code

                                                    Nothing -> if null ls then
                                                                   error "Invalid code." --we don't have bits and k is not a valid code
                                                               else
                                                                   decode' (k ++ [head ls]) (tail ls) ---this is not valid code yet, we add next bit to code



--findCharCode [('a',[0]),('b',[1,0]),('c',[1,1])] [1,0]
--returns Just ('b',[1,0])

--findCharCode [('a',[0]),('b',[1,0]),('c',[1,1])] [0,1]
--returns Nothing
findCharCode :: Code -> Assoc -> Maybe (Char, Code)
findCharCode k = List.find(\x -> snd x == k)
