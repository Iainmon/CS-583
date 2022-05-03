{-# LANGUAGE NamedFieldPuns, FlexibleInstances #-}
data L ag at 
  = Prim at
  | Neg (L ag at) 
  | And (L ag at) (L ag at) 
  | Know ag (L ag at)

instance (Show ag,Show at) => Show (L ag at) where
  show (Prim p) = show p
  show (Neg p)  = "¬ " ++ show p
  show (And p1 p2) = "(" ++ show p1 ++ " ⋀ " ++ show p2 ++ ")"
  show (Know a p)  = "K<" ++ show a ++ ">[" ++ show p ++ "]"

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


-- instance Show (L Char String) where
--   show (Prim p) = p
--   show (Neg p)  = "¬ " ++ show p
--   show (And p1 p2) = "(" ++ show p1 ++ " ⋀ " ++ show p2 ++ ")"
--   show (Know a p)  = "K<" ++ a:[] ++ ">[" ++ show p ++ "]"

data KMAgent = A | B deriving Eq
instance Show KMAgent where { show A = "a"; show B = "b" }

data KMPrim  = T_a | T_b deriving Eq
instance Show KMPrim where { show T_a = "t_a"; show T_b = "t_b" }

data KMState = S | U | V | W deriving Eq
instance Show KMState where { show S = "s"; show U = "u"; show V = "v"; show W = "w"; }


kModel :: KripkekModel KMAgent KMPrim KMState
kModel = M {agents,prims,states,accessibility,valuation}
  where 
        agents = [A,B]
        prims = [T_a,T_b]
        states = [S,U,V,W]

        accessibility A = [(s1,s2) | s1 <- [S,U], s2 <- [S,U]] ++ [(s1,s2) | s1 <- [W,V], s2 <- [W,V]]
        accessibility B = [(s1,s2) | s1 <- [S,W], s2 <- [S,W]] ++ [(s1,s2) | s1 <- [U,V], s2 <- [U,V]]

        valuation S T_a = False
        valuation S T_b = True
        valuation U T_a = False
        valuation U T_b = False
        valuation V T_a = True
        valuation V T_b = False
        valuation W T_a = True
        valuation W T_b = True

tests = concat [
        [(Know ag (Prim pr))       
        ,(Know ag (Neg $ Prim pr))
        ,(Neg $ Know ag (Prim pr))] 
        | pr <- prims kModel, ag <- agents kModel]

runT = do
  let prints = do 
      test    <- tests
      state   <- states kModel
      let res = sem kModel state test
      let padding = (if res then " " else "") ++ "    "
      return $ putStrLn ("[" ++ show res ++ "]" ++padding++ "M," ++ show state ++ " ⊨ " ++ show test)
  sequence prints
  return ()

