module Occurrence2tree(occurrence2tree) where

import Types

--creates a Huffman tree with respect to char occurrences
--occurrence2tree  [('b',1),('c',1),('a',3)]
--returns Node 5 (Node 2 (Leaf 'b' 1) (Leaf 'c' 1)) (Leaf 'a' 3)
occurrence2tree :: Occurrence -> Tree
occurrence2tree = trees2tree . occurrence2trees 



--converts a char with occurence to a tree
occurrence2trees :: Occurrence -> [Tree]
occurrence2trees = map $ uncurry Leaf

--combines trees until there remains just one tree
trees2tree :: [Tree] -> Tree
trees2tree [] = Leaf '\0' 0
trees2tree [t] = t
trees2tree ts = trees2tree $ mergeTree ts

--merges first two trees and preserves sort invariant
mergeTree :: [Tree] -> [Tree]
mergeTree (t1 : t2 : ts) = insert (combinePair t1 t2) ts


--inserts a tree to a collection which preserves sort invariant
insert :: Tree -> [Tree] -> [Tree]
insert t1 [] = [t1]
insert t1 (t2 : ts) | v2 <= v1   = t2 : insert t1 ts
                  | otherwise  = t1 : t2 : ts
                    where 
                        v1 = value t1
                        v2 = value t2

--creates from two trees one new super tree
--whose occurrence is equal to sum of occurrences of trees t1 and t2
combinePair :: Tree -> Tree -> Tree
combinePair t1 t2 = Node (v1 + v2) t1 t2
                        where
                        v1 = value t1
                        v2 = value t2

value :: Tree -> Int
value (Leaf _ n) = n
value (Node n _ _) = n
