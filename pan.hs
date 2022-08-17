


type Point = (Float,Float)

type Image a = Point -> a

type Region = Image Bool


vstrip :: Region
vstrip (x,y) = abs x >= (1/2)


type PolarPoint = (Float,Float)



type Frac = Float

type Color = (Frac,Frac,Frac,Frac)



type ImageC = Image Color


lift3 :: (a -> b -> c -> d) -> (p -> a) -> (p -> b) -> (p -> c) -> p -> d
lift3 f a b c x = f (a x) (b x) (c x)

lift2 :: (a -> b -> c) -> (p -> a) -> (p -> b) -> p -> c
lift2 f a b x = f (a x) (b x)

lift :: (a -> b) -> (p -> a) -> p -> b
lift f a x = f (a x)



cond :: Image Bool -> Image a -> Image a -> Image a
cond = lift3 (\a b c -> if a then b else c)
-- cond d a b p = if d p then a p else b p



type Transform = Point -> Point

type Vector = (Float,Float)

type Filter a = Image a -> Image a





type Shape a = Image (Either a a)

type Partition a = Image a -> Shape a

type Figure a = Shape a -> Image a
type Mask a = Partition a -> Filter a


isolate :: (a -> Bool) -> Image a -> Region
isolate = lift

selectShape :: Region -> Image a -> Shape a
selectShape = lift2 b2e
  where b2e True = Right
        b2e _    = Left

toPart :: Region -> Partition a
toPart = selectShape

select :: (a -> Bool) -> Partition a
select p f = toPart (isolate p f) f

toMask :: Filter a -> Mask a
toMask flt pt f p | (Right _) <- pt f p = flt f p
                  | otherwise           = f p



-- Combines two partitions and switches a point's L/R channel if they disagree.
--   (\p -> Right 1) >*> (\p -> Left 1) 
(>*>) :: Partition a -> Partition a -> Partition a
-- (>*>) :: (Image a -> Shape a) -> (Image a -> Shape a) -> Image a -> Shape a
-- (>*>) :: (Image a -> Image (Either a a)) -> (Image a -> Image (Either a a)) -> Image a -> Image (Either a a)
-- (>*>) :: (Image a -> Image (Either a a)) -> (Image a -> Image (Either a a)) 
--                                          -> Image a 
--                                          -> Point 
--                                          -> (Either a a)
(>*>) p1 p2 f p = case p1 f p of {
                    (Left a) -> case p2 (lift (leftOr a) (p1 f)) p of {
                                    (Left x) -> Left x;
                                    _        -> Right $ f p
                                  };
                    (Right a) -> case p2 (lift (rightOr a) (p1 f)) p of {
                                    (Right x) -> Right x;
                                    _         -> Left $ f p
                                  };
                    }
  where leftOr  _ (Left x)  = x
        leftOr  a _         = a
        rightOr _ (Right x) = x
        rightOr a _         = a


ifmap :: (a -> b) -> Image a -> Image b
ifmap = lift

ireturn :: a -> Image a
ireturn = const

ibind :: Image a -> (a -> Image b) -> Image b
ibind f g p = g (f p) p

freturn :: a -> Filter a
freturn = const . ireturn 

fbind :: Filter a -> (a -> Filter b) -> Filter b
fbind = undefined 



-- ffmap :: (a -> b) -> Filter a -> Filter b
-- ffmap :: (a -> b) -> (Image a -> Image a) -> (Image b -> Image b)
-- ffmap :: (a -> b) -> ((Point -> a) -> (Point -> a)) -> (Point -> b) -> (Point -> b)

-- ffmap :: (a -> b) -> Filter a -> Filter b
-- ffmap g f h p = 


-- newtype Image' a = I (Image a)

-- newtype Filter' a = F (Image' a -> Image' a)

-- instance Functor Image' where
--   -- fmap :: (a -> b) -> Image a -> Image b
--   fmap g (I f) = I $ g . f

-- instance Functor Filter' where
--   -- fmap :: (a -> b) -> Filter a -> Filter b
--   -- fmap :: (a -> b) -> (Image a -> Image a) -> (Image b -> Image b)
--   fmap g (F f) = undefined 

-- ffmap :: (a -> b) -> Filter' a -> Filter' b