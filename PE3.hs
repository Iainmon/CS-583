-- data BinTree a 
--   = Branch a (BinTree a) (BinTree a)
--   | Empty
--   deriving (Show)
-- foldT :: T1 -> T2 -> BinTree a -> T3
-- takes two parameters (a function and a value)
-- and converts a binary tree of type `BinTree a`
-- to the type `T3`. 
-- Find the most general type of `foldT`.
-- Should be able to write 
--    `sumT` - computes sum of values in tree
-- and 
--    `maxT` - computes max of values in tree

-- fldl f acc [] = acc
-- fldl f acc (x:xs) = fldl f (f acc x) xs

-- fldr f ini [] = ini
-- fldr f ini (x:xs) = f x (fldr f ini xs)


-- foldTr :: (a -> b -> b -> b) -> b -> BinTree a -> b
-- foldTr _ z Empty            = z
-- foldTr f z (Branch a tl tr) = f a (foldTr f z tl) (foldTr f z tr)

-- foldTl :: (b -> a -> b) -> b -> BinTree a -> b
-- foldTl _ z Empty            = z
-- foldTl f z (Branch a tl tr) = foldTl f (foldTl f (f z a) tl) tr

-- http://hilite.me/
data BinTree a 
  = Branch a (BinTree a) (BinTree a)
  | Empty
  deriving Show

foldT :: (b -> a -> b) -> b -> BinTree a -> b
foldT _ z Empty            = z
foldT f z (Branch a tl tr) = foldT f (foldT f (f z a) tl) tr

sumT :: BinTree Int -> Int
sumT = foldT (+) 0

maxT :: BinTree Int -> Int
maxT tr = foldT max (-(sumT tr)) tr

lf a = Branch a Empty Empty
br = Branch

tree :: BinTree Int
tree = br 2 (lf 1) (lf 3)


levels ::  BinTree a -> [[a]]
levels Empty            = [[]]
levels (Branch a tl tr) = [a] : zipWith (++) (levels tl) (levels tr)

{--
ghci> sumT tree
6
ghci> maxT tree
3
ghci> levels tree
[[2],[1,3],[]]
ghci> tree
Branch 2 (Branch 1 Empty Empty) (Branch 3 Empty Empty)
--}