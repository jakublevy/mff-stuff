module Tree2assoc(tree2assoc) where

import Types

tree2assoc :: Tree -> Assoc
tree2assoc = convert []



--gradually traverses the whole tree and constructs code for each letter
convert :: Code -> Tree -> Assoc
convert k (Leaf c _) = [(c,k)]    --left branch           --right branch
convert k (Node _ t1 t2) = convert (k ++ [0]) t1 ++ convert (k ++ [1]) t2
