import Control.Monad (mapM_, when)

data Commented a = Computation a [String] deriving Show

{- vsechna undefined nahradte vlastni implementaci -}
comment :: String -> Commented ()
comment x = Computation () [x]

runCommented :: Commented a -> (a, [String])
runCommented (Computation r xs) = (r,xs)

instance Functor Commented where
  -- fmap :: (a->b) -> Commented a -> Commented b
  fmap f (Computation r xs) = Computation (f r) xs

instance Applicative Commented where
  -- pure :: a -> Commented a
  pure x = Computation x []

  -- (<*>) :: Commented (a -> b) -> Commented a -> Commented b
  (Computation g ys) <*> ca = fmap g ca
  --(Computation g ys) <*> (Computation r xs) = Computation (g r) (xs ++ ys)

instance Monad Commented where
  -- return :: a -> Commented a
  return = pure

  -- (>>=) :: Commented a -> (a -> Commented b) -> Commented b
  (Computation r xs) >>= g = Computation s (xs ++ ys)
                          where (Computation s ys) = g r

{- Testovani -}
solutions a b c = do
  when (a == 0) $ comment "Ajaj, rovnice je ve skutecnosti linearni."
  let d = b ^ 2 - 4 * a * c
  comment $ "Diskriminant je " ++ show d
  if (d < 0) then do
      comment "Nemame reseni!"
      return []
  else do
      comment "Parada, mame alespon jedno reseni!"
      return $ (\op -> (-b `op` sqrt d) / (2 * a)) `map` [(+), (-)]

twoSolutions a b1 b2 c = do
  sol1 <- solutions a b1 c
  comment $ "Prvni rovnice ma " ++ show (length sol1) ++ " reseni"
  sol2 <- solutions a b2 c
  comment $ "Druha rovnice ma " ++ show (length sol1) ++ " reseni"
  return $ sol1 ++ sol2

{- jednoduchy komentovany foldr (negenericky, fungujici jen na seznamy) -}
cFoldr :: Show a => (a -> a -> a) -> a -> [a] -> Commented a
cFoldr _ x [] = do
                comment $ "pocatecni akumulator: " ++ show x
                return x

cFoldr f a (x : xs) = do
                    a' <- cFoldr f a xs
                    comment $ "Folduji prvek: " ++ show x ++ " a akumulator: " ++ show a'
                    return $ f x a'

main = do
  let (a, comments) = runCommented $ twoSolutions 5 (-3) (-4) (-1)
  mapM_ putStrLn comments
  putStrLn . ("Vysledek twoSolutions: " ++) . show $ a
  let (a, comments) = runCommented $ cFoldr (*) 1 [1 .. 10]
  mapM_ putStrLn comments
  putStrLn . ("Vysledek foldr: " ++) . show $ a
  