


-- data Parser a = P (String -> [(a,String)])



data State s a = State (s -> (a,s))


instance Functor (State s) where
  fmap f (State st) = State $ \s->let (x,s') = st s in (f x,s')
instance Applicative (State s) where
  pure x = State (\s->(x,s))
  State st_f <*> State st_s =
      State (\s->let (f,s') = st_f s
                     (x,s'') = st_s s'
                  in (f x,s''))
instance Monad (State s) where
  -- return = pure
  State c >>= f = State (\s->let (x,s') = c s
                                 State d = f x



run :: State s a -> s -> (a,s)
run (State f) = f

initState :: s -> State s ()
initState s = State $ \_ -> ((),s)

onState :: (s -> s) -> State s ()
onState f = State $ \s -> ((),f s)

fromState :: (s -> a) -> State s a
fromState f = State $ \s -> (f s,s)

get :: State s s
get = fromState id





type Counter a = State Int a

reset :: Counter ()
reset = initState 0
-- reset = return 0

incr :: Counter ()
incr = onState (+1)

add :: Int -> Counter ()
add n = onState (+n)



