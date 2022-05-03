{-# LANGUAGE NamedFieldPuns, FlexibleInstances #-}
data L ag at 
  = Prim at
  | Neg (L ag at) 
  | And (L ag at) (L ag at) 
  | Know ag (L ag at)

-- instance (Show ag,Show at) => Show (L ag at) where
--   show (Prim p) = show p
--   show (Neg p)  = "¬ " ++ show p
--   show (And p1 p2) = "(" ++ show p1 ++ " ⋀ " ++ show p2 ++ ")"
--   show (Know a p)  = "K<" ++ show a ++ ">[" ++ show p ++ "]"

type Collection a = [a] -- Set a
isin :: Eq a => a -> Collection a -> Bool
isin = undefined
union  :: Eq a => Collection a -> Collection a -> Collection a
union = undefined
toList :: Collection a -> [a]
toList = undefined

type R agent state = agent -> Collection (state,state)
type V state prim  = state -> prim -> Bool

data KripkekModel agent prim state
  = M { agents :: Collection agent
      , prims  :: Collection prim
      , states :: Collection state
      , accessibility :: R agent state
      , valuation :: V state prim 
      }


sem :: Eq state => KripkekModel agent prim state -> state -> L agent prim -> Bool
sem model@(M{agents,prims,states,accessibility,valuation}) s phi = sem' phi
  where models = sem model -- entails
        sem' (Prim pr)   = valuation s pr
        sem' (Neg phi)   = not $ models s phi
        sem' (And p1 p2) = models s p1 && models s p2
        sem' (Know a p)  = and [models t p | (s',t) <- accessibility a, s == s']


instance Show (L Char String) where
  show (Prim p) = p
  show (Neg p)  = "¬ " ++ show p
  show (And p1 p2) = "(" ++ show p1 ++ " ⋀ " ++ show p2 ++ ")"
  show (Know a p)  = "K<" ++ a:[] ++ ">[" ++ show p ++ "]"


kModel :: KripkekModel Char String Char
kModel = M {agents,prims,states,accessibility,valuation}
  where 
        agents = "ab"
        prims = ["t_" ++ (a:"") | a <- agents]
        states = "suvw"

        accessibility 'a' = [(s1,s2) | s1 <- "su", s2 <- "su"] ++ [(s1,s2) | s1 <- "wv", s2 <- "wv"]
        accessibility 'b' = [(s1,s2) | s1 <- "sw", s2 <- "sw"] ++ [(s1,s2) | s1 <- "uv", s2 <- "uv"]

        valuation 's' "t_a" = False
        valuation 's' "t_b" = True
        valuation 'u' "t_a" = False
        valuation 'u' "t_b" = False
        valuation 'v' "t_a" = True
        valuation 'v' "t_b" = False
        valuation 'w' "t_a" = True
        valuation 'w' "t_b" = True

tests = [(Know 'a' (Prim "t_a"))       
        ,(Know 'a' (Neg $ Prim "t_a"))
        ,(Know 'a' (Prim "t_b"))       
        ,(Know 'a' (Neg $ Prim "t_b")) 
        ,(Neg $ Know 'a' (Prim "t_b"))
        ]

runT = do
  let prints = do 
      test <- tests
      let res = sem kModel 'v' test
      let padding = (if res then " " else "") ++ "    "
      return $ putStrLn ("[" ++ show res ++ "]" ++padding++ "M,v ⊨ " ++ show test)
  sequence prints
  return ()

