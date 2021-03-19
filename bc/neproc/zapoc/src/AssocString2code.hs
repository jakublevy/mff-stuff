module AssocString2code(assocString2code) where

import Types
import qualified Data.List as List

--assocString2code [('a', [1,1,0]), ('b',[1,1,1])] "ab"
--returns [1,1,0,1,1,1]
assocString2code :: Assoc -> String -> Code
assocString2code m = concat . map (findCode m)



--findCode [('a', [1,1,0]), ('b',[1,1,1])] 'a'
--returns [1,1,0]
findCode :: Assoc -> Char -> Code
findCode m c = snd $ removeMaybe $ List.find (\x -> fst x == c) m

--Removes a contructor for found value.
--possibly reports error, if no value was found
removeMaybe :: Maybe a -> a
removeMaybe (Just x) = x
removeMaybe Nothing = error "Code was not found in lookup table."
