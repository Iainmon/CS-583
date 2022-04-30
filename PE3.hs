data BinTree a 
  = Branch a (BinTree a) (BinTree a)
  | Empty
  

foldT :: T1 -> T2 -> BinTree a -> T3

foldT = undefined

levels ::  BinTree a -> [[a]]
levels = undefined
