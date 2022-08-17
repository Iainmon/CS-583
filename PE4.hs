-- import Data.List as List
-- import Data.Foldable

-- data RoseTree' a = Leaf a | Branch a [RoseTree' a]

-- class Spreadable a where
--     spread :: a -> [a]

-- instance Spreadable [a] where
--     spread [] = []
--     spread xs = xs : (spread . tail $ xs)

-- instance Spreadable (RoseTree' a) where
--     spread (Leaf a) = [Leaf a]
--     spread (Branch a as) = Branch a as : foldl' (++) [] (map spread as)

-- root :: RoseTree' a -> a
-- root (Branch a _) = a
-- root (Leaf a)     = a

-- instance Foldable RoseTree' where
--     -- foldr :: (a -> b -> b) -> b -> RoseTree' a -> b
--     foldr f z (Leaf a) = a `f` z
--     foldr f z (Branch a as) = a `f` List.foldl' (foldr f) z as
--     toList = map root . spread




-- fldr :: (b -> a -> b) -> b -> [a] -> b
-- fldl f acc [] = acc
-- fldl f acc (x:xs) = fldl f (f acc x) xs
--   fldr (+) 0 [1,2,3,4,5] == (((((0 + 1) + 2) + 3) + 4) + 5)

-- fldr :: (a -> b -> b) -> b -> [a] -> b
-- fldr f ini [] = ini
-- fldr f ini (x:xs) = f x (fldr f ini xs)
--   fldr (+) 0 [1,2,3,4,5] == (1 + (2 + (3 + (4 + (5 + 0)))))
--   fldr (++) [] [[1,2], [3,4], [5]] == [1,2,3,4,5]

-- data RoseTree a = Node a [RoseTree a] deriving Show

-- subtr a as = Node a $ map (`Node` []) as
-- tree1 = subtr 2 [1,3]
-- tree2 = subtr 5 ([1..4] ++ [6..10])

foldR :: (a -> b -> b) -> b -> RoseTree a -> b
foldR f z (Node a []) = f a z
foldR f z (Node a ts) = f a (foldr (flip (foldR f)) z ts)

-- foldR' :: (a -> b -> b) -> b -> RoseTree a -> b
-- foldR' f z (Node a []) = f a z
-- foldR' f z (Node a ts) = f a (foldl (foldR' f) z ts) -- foldl difference!


-- foldR'' :: (a -> [b] -> b) -> [b] -> RoseTree a -> b
-- foldR'' f z (Node a []) = f a z
-- foldR'' f z (Node a ts) = f a (map (foldR'' f z) ts)

-- mapR :: (a -> b) -> RoseTree a -> RoseTree b

-- mapR :: (t -> a) -> RoseTree t -> [RoseTree a]
-- mapR f rt = foldR (\a st -> [Node (f a) st]) [] rt




data RoseTree a = Node a [RoseTree a] deriving Show

subtr a as = Node a $ map (`Node` []) as
tree1 = subtr 2 [1,3]

-- foldR :: (a -> [b] -> b) -> [b] -> RoseTree a -> b
-- foldR f z (Node a []) = f a z
-- foldR f z (Node a ts) = f a (map (foldR f z) ts)

-- mapR :: (a -> b) -> RoseTree a -> RoseTree b
-- mapR f = foldR (Node . f) []

-- dfs :: RoseTree a -> [a]
-- dfs = foldR (flip (flip (:) . concat)) []

