{-# LANGUAGE TypeSynonymInstances #-}


import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (traverse)

data Prop
  = And Prop Prop
  | Or  Prop Prop
  | Imp Prop Prop
  | Not Prop
  | P Char

instance Show Prop where
  show (And p q) = show p ++ " /\\ " ++ show q
  show (Or p q)  = show p ++ " \\/ " ++ show q
  show (Imp p q) = show p ++ " => " ++ show q
  show (P p)     = p:[]
  show (Not p)   = "¬ " ++ show p

(/\) = And
(\/) = Or
(-->) = Imp
neg = Not

prims = map P "pqr"
(p:q:r:_) = prims




value :: [(Char,Bool)] -> Char -> Bool
value [] _                      = undefined
value ((c,b):cs) c' | c == c'   = b
                    | otherwise = value cs c'
                    
cartith :: Int -> [a] -> [[a]]
cartith 0 _  = []
cartith 1 ls = [[x] | x <- ls]
cartith n ls = let subs = cartith (n - 1) ls in [x : xs |  x <- ls, xs <- subs]

combs :: [a] -> [[a]]
combs ls = cartith (length ls) ls

worlds :: [a] -> [[(a, Bool)]]
worlds []     = [[]]
worlds [p]    = [[(p,True)],[(p,False)]]
worlds (p:ps) = [(p,b):w | b <- [True,False], w <- worlds ps]

-- car (x:[]) = fmap pure x
-- car (x:xs) = [y  ys | y <- xs, ys <- car xs]

-- car :: (Traversable t, Applicative f) => t (f a) -> f (t (f a))
-- car xs = fmap (foldl mappend mempty) . sequenceA . traverse (fmap pure) xs -- interesting
-- car :: (Ord (t [a]), Traversable t) => t (Set a) -> Set (t [a])
car :: (Ord (t b), Traversable t) => t (Set b) -> Set (t b)
car = Set.fromList . traverse Set.toList
-- carWith :: (Ord (t b), Traversable t) => (a -> t b) -> t (Set a) -> Set (t b)
-- carWith :: (Ord (t b), Traversable t) => (a -> [b] -> [b]) -> t (Set a) -> Set (t b)
carWith :: (Traversable t, Applicative f, Monoid (f b)) => (a -> f b -> f b) -> t (Set a) -> f (t b)
carWith f = traverse ( foldl (flip f) mempty . Set.toList)


-- -- takeFromEach []     = []
-- takeFromEach [] n = map (const []) 
-- takeFromEach (x:xs) n = [x : ys | ys <- takeFromEach xs]

choices :: [[a]] -> [[a]]
choices []     = []
choices [x]    = map return x
choices (x:xs) = [y : ys | y <- x, ys <- choices xs]


-- products :: (Ord a,Foldable f,Ord (f a)) => f (Set a) -> Set (f a)
products :: (Ord a) => [Set a] -> Set [a]
products = Set.fromList . choices . map Set.toList

-- products' :: (Foldable t, Ord a) => t (Set a) -> Set [a]
-- products' = foldr (\a -> Set.map (uncurry (:)) . Set.cartesianProduct a) (Set.singleton [])

sem :: Prop -> [(Char,Bool)] -> Bool
sem (And p q) w = and $ map (flip sem w) [p,q]
sem (Or p q)  w = or $ map (flip sem w) [p,q]
sem (Imp p q) w = sem (neg p \/ q) w
sem (Not p) w   = not $ sem p w
sem (P p) w     = value w p

prob2a = (p --> (q --> r))



exmtr = Node 0 (Node 0 (Node 0 (Leaf 7) (Leaf 6)) (Node 0 (Leaf 8) (Leaf (-10)))) (Node 0 (Node 0 (Leaf 2) (Leaf 1)) (Node 0 (Leaf (-15)) (Leaf (-10))))