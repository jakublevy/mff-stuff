module Types where

--one bit of code
--0 = left branch
--1 = right branch
type Bit = Int

--character code
type Code = [Bit]

--(char, code)
--('a',[1,1,0])
type Assoc = [(Char, Code)]

--[('a',2),('b',3)] 
--means that char 'a' occurred twice and char 'b' 3 times
type Occurrence = [(Char, Int)]

               --char in leaf  --number of occurrences
data Tree = Leaf Char             Int |

         --(inner nodes)
         --number of occurrences
            Node Int           Tree Tree deriving Show

