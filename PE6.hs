class Tree t where
  root :: t a -> a
  subtrees :: t a -> [t a]
  isEmpty :: t a -> Bool

spread :: Tree t => t a -> [[t a]]
spread tr = [tr] : filter (not . null) (spread' tr)
  where spread' tr | isEmpty tr = []
                   | otherwise  = subtrees tr : concat [spread' s | s <- subtrees tr] -- [tr] : concatMap spread (subtrees tr)

  
levels :: Tree t => t a -> [[a]]
levels tr = [map root lev | lev <- spread tr]


-- foldT :: Tree t => (b -> a -> b) -> b -> t a -> b
-- foldT f z = foldl f z . concat . levels

-- dfs :: Tree t => t a -> [a]
-- -- dfs = reverse . foldT (flip (:)) []
-- dfs = concat . levels

foldT :: Tree t => (a -> b -> b) -> b -> t a -> b
foldT f z = foldr f z . concat . levels

dfs :: Tree t => t a -> [a]
dfs = foldT (:) []


sumT :: (Num a,Tree t) => t a -> a
sumT = foldT (+) 0


data RoseTree a = Node a [RoseTree a] deriving Show

instance Tree RoseTree where
  root (Node a _) = a
  subtrees (Node _ st) = st
  isEmpty = const False

tr :: a -> [a] -> RoseTree a
tr a as = Node a [Node a' [] | a' <- as]

t1 = tr 2 [1,3]