



data Dist val = D [(val,Double)] deriving Show


instance Functor Dist where
  fmap f (D dist) = D [(f x,d) | (x,d) <- dist]




testDist1 :: Dist Int
testDist1 = D [(0,0.5),(1,0.5)]


