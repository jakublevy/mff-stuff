module String2occurrence(string2occurrence) where

import Types
import qualified Data.List as List

--string2occurrence "ababa"
--returns [('a',2),('b',3)]
string2occurrence :: String -> Occurrence
string2occurrence = List.sortBy (\(_,y) (_,y') -> compare y y') . map (\x -> (head x, length x)) . List.group . List.sort
