

class Tree t where
  root :: t a -> a
  subtrees :: t a -> [t a]
  isEmpty :: t a -> Bool

data BinTree a 
  = Branch a (BinTree a) (BinTree a)
  | Empty

instance Tree BinTree where
  root (Branch a _ _)     = a
  root _ = undefined
  subtrees (Branch _ l r) = [l,r]
  subtrees _ = undefined
  isEmpty Empty = True
  isEmpty _     = False



data RoseTree a = Node a [RoseTree a]

instance Tree RoseTree where
  root (Node a _) = a
  subtrees (Node _ st) = st
  isEmpty = null . subtrees

instance Tree [] where
  root = head
  subtrees = return . tail -- assumes a list has only one or no child. 
  isEmpty = null
