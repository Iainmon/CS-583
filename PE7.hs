data Decision a = Certain a | Choice [a]


instance Functor Decision where
  fmap f (Certain a) = Certain (f a)
  fmap f (Choice as) = Choice (map (fmap f) as)

instance Monad Decision where
  return a = Certain a
  (Certain a) >>= f = case f a of
                        Certain b -> Certain b
                        Choice bs -> Certain 
