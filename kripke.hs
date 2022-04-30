{-# LANGUAGE MultiParamTypeClasses, GADTs, TypeFamilies, FlexibleInstances #-}

-- data Modality ag = K ag
-- data Lang ag at 
--   = Prim at 
--   | And (Lang ag at) (Lang ag at)
--   | Neg (Lang ag at)
--   | MOp (Modality ag) (Lang ag at)
data Modality ag where
    K :: ag -> Modality ag

data Lang ag at where
    Prim :: at -> Lang ag at
    And  :: Lang ag at -> Lang ag at -> Lang ag at
    Neg  :: Lang ag at -> Lang ag at
    MOp  :: Modality ag -> Lang ag at -> Lang ag at

type Agent = Int

type KnowLang at = Lang Agent at


class Logic (log :: * -> *) at where
  atoms :: log at -> [at]
  follows :: log at -> [log at]

instance Logic (Lang Agent) at where
  atoms (Prim p)    = [p]
  atoms (And p1 p2) = atoms p1 ++ atoms p2
  atoms (Neg p)     = atoms p
  atoms (MOp _ p)   = atoms p
  follows = undefined

-- instance Semantic KnowLang Bool where
--   teval w (Prim p)    = w p
--   teval w (And p1 p2) = teval w p1 && teval w p2
--   teval w (Neg p)     = not (teval w p)
--   teval w _           = undefined




-- class Logic log => Semantic log tv where
--   teval :: (at -> tv) -> log at -> tv


class (Logic log st) => Model model st log where
  entails :: model st at tv -> log at -> tv




data Color = RGB (Int,Int,Int) | RGBA (Int,Int,Int,Int)

type Transperancy = Maybe Int 


type Color = 
