{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}




-- class Collects e ce where
--   empty  :: ce
--   insert :: e -> ce -> ce
--   member :: e -> ce -> Bool




class Translate a b | b -> a where
  unitR :: b
  trans :: a -> b

data TM = JustT Int | TNothing

instance Translate TM Int where
  unitR = 0
  trans (JustT n) = n
  trans TNothing  = 0

-- instance Translate Int Int where
--   unitR = 1
--   trans = id


f :: Translate a Int => Int
f = unitR

