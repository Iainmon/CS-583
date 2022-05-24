{-# LANGUAGE NamedFieldPuns, FlexibleInstances, TupleSections #-}

{-# LANGUAGE NamedFieldPuns, FlexibleInstances, TupleSections #-}

{-# LANGUAGE NamedFieldPuns, FlexibleInstances, TupleSections #-}

{-# LANGUAGE NamedFieldPuns, FlexibleInstances, TupleSections #-}
{-# LANGUAGE NamedFieldPuns, FlexibleInstances, TupleSections #-}

{-# LANGUAGE NamedFieldPuns, FlexibleInstances, TupleSections #-}

{-# LANGUAGE NamedFieldPuns, FlexibleInstances, TupleSections #-}

{-# LANGUAGE NamedFieldPuns, FlexibleInstances, TupleSections #-}
import Data.List (inits, tails, nub, permutations, nubBy, isPrefixOf)

import Data.Set (Set)
import qualified Data.Set as Set

data L ag at 
  = Prim at
  | Neg (L ag at) 
  | And (L ag at) (L ag at) 
  | Know ag (L ag at)
  | Believe ag (L ag at)

instance (Show ag,Show at) => Show (L ag at) where
  show (Prim p) = show p
  show (Neg p)  = "¬ " ++ show p
  show (And p1 p2) = "(" ++ show p1 ++ " ⋀ " ++ show p2 ++ ")"
  show (Know a p)  = "K<" ++ show a ++ ">[" ++ show p ++ "]"
  show (Believe a p) = "B<" ++ show a ++ ">[" ++ show p ++ "]"

agentsUsed :: L ag at -> [ag]
agentsUsed (Prim _) = []
agentsUsed (Neg phi) = agentsUsed phi
agentsUsed (And p1 p2) = agentsUsed p1 ++ agentsUsed p2
agentsUsed (Know a p) = a : agentsUsed p
agentsUsed _ = undefined

primsUsed :: L ag at -> [at]
primsUsed (Prim p) = [p]
primsUsed (Neg phi) = primsUsed phi
primsUsed (And p1 p2) = primsUsed p1 ++ primsUsed p2
primsUsed (Know _ p) = primsUsed p
primsUsed _ = undefined

type Collection a = [a] -- Set a
isin :: Eq a => a -> Collection a -> Bool
isin = undefined
union  :: Eq a => Collection a -> Collection a -> Collection a
union = undefined
toList :: Collection a -> [a]
toList = undefined

-- type State = Map PrimProp Bool

type R agent state = agent -> Collection (state,state)
type V state prim  = state -> prim -> Bool

data KripkeModel agent prim state
  = M { agents :: Collection agent
      , prims  :: Collection prim
      , states :: Collection state
      , accessibility :: R agent state
      , valuation :: V state prim 
      }

instance (Show agent,Show prim,Show state) => Show (KripkeModel agent prim state) where
  show M{agents,prims,states,accessibility,valuation} = "Agents: " ++ show agents ++ "\nPrims: " ++ show prims ++ "\nStates: " ++ show states ++ "\nAccess: " ++ show [(a,rel) | a <- agents, rel <- accessibility a]

sem :: Eq state => KripkeModel agent prim state -> state -> L agent prim -> Bool
sem model@(M{agents,prims,states,accessibility,valuation}) s phi = sem' phi
  where models = sem model -- entails
        sem' (Prim pr)   = valuation s pr
        sem' (Neg phi)   = not $ models s phi
        sem' (And p1 p2) = models s p1 && models s p2
        sem' (Know a p)  = and [models t p | (s',t) <- accessibility a, s == s']
        sem' (Believe a p)  = and [models t p | (s',t) <- accessibility a, s == s']



type State prim = ([prim],[prim]) -- (truths, falsehoods)

realWorld = ([],[])

decide :: Eq prim => State prim -> prim -> Bool
decide (trus,flss) p | p `elem` trus = True
                     | p `elem` flss = False
                     | trus == [] && flss == [] = True 
                     | otherwise     = undefined

partitions :: [a] -> [[[a]]]
partitions = foldr (\x r -> r >>= bloat x) [[]]
  where bloat x  []      = [[[x]]]
        bloat x (xs:xss) = ((x:xs):xss) : map (xs:) (bloat x xss)

bipartitions :: [a] -> [[[a]]]
bipartitions = filter ((==) 2 . length) . partitions

consStates :: Eq prim => [prim] -> [State prim]
-- consStates xs = [(as,bs) | [as,bs] <- bipartitions xs]
consStates xs = [(a,b) | b <- subsets xs,
                         a <- subsets xs,
                         a ++ b `elem` permutations xs]


consModel :: (Eq agent, Eq prim) => [agent] -> [prim] -> R agent (State prim) -> KripkeModel agent prim (State prim)
consModel ags prms accessR = M { agents       = ags
                              , prims         = prms
                              , states        = consStates prms
                              , valuation     = decide
                              , accessibility = accessR
                              }
            

subsets :: [a] -> [[a]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

consRelations :: Eq state => [state] -> state -> [[(state,state)]]
consRelations states s = nub $ map (map (s,) . nub) $ subsets states

functionify :: Eq a => [(a,b)] -> a -> b
functionify [] _ = undefined
functionify ((x,y):xys) x' | x == x'   = y
                           | otherwise = functionify xys x'


type KripkeStar agent prim = KripkeModel agent prim (State prim)

consModels :: (Eq agent, Eq prim) => [agent] -> [prim] -> [KripkeStar agent prim]
consModels ags prms = map (consModel ags prms) relations
  where relations     = map functionify  $ filter valid $ subsets relationPairs
        relationPairs = nub [(a,rel) | a <- ags, rel <- consRelations states realWorld]
        valid rel = length ags == length (nub $ map fst rel)
        states = consStates prms


findModels :: (Eq ag, Eq at) => L ag at -> [KripkeStar ag at]
findModels formula = filter satisfies models
  where models = consModels (agentsUsed formula) (primsUsed formula)
        satisfies m = sem m realWorld formula


sameRelation :: (Eq ag, Eq at) => KripkeStar ag at -> KripkeStar ag at -> Bool
sameRelation k1 k2 = nub [(a,s) | a <- agents k1,s <- accessibility k1 a] == nub [(a,s) | a <- agents k2,s <- accessibility k2 a]


formula = (Know "a" (Prim "ta") `And` (Prim "tb")) `And` (Know "b" (Neg $ And (Neg $ Prim "ta") (Neg $ Prim "tb")))


allModels = consModels (agentsUsed formula) (primsUsed formula)

foundModels = nubBy sameRelation $ findModels formula



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


kModel :: KripkeModel KMAgent KMPrim KMState
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

islandKModel :: Int -> KripkeModel Int Int [Int]
islandKModel n = M {agents,prims,states,accessibility,valuation}
  where agents = [1..n]
        prims  = [1..n]
        s0 = map (const 1) [1..n]
        states = s0 : [[if j == i then 0 else 1 | j <- [1..n]] | i <- [1..n]] -- [0..round (2.0 ** (fromIntegral n :: Float))]
        accessibility m = [(s0,s0),(s0,[if j == m then 0 else 1 | j <- [1..n]]),([if j == m then 0 else 1 | j <- [1..n]],s0),([if j == m then 0 else 1 | j <- [1..n]],[if j == m then 0 else 1 | j <- [1..n]])]
        valuation s k = if s!!(k-1) == 1 then True else False
        -- the binary number with the n-th bit 1, and rest all zeros, is 2^n
        -- logb x y = round $ logBase (fromIntegral x :: Float) (fromIntegral y :: Float)

-- tests m = concat [
--         [(Know ag (Prim pr))       
--         ,(Know ag (Neg $ Prim pr))
--         ,(Neg $ Know ag (Prim pr))
--         ,(Believe ag (Prim pr))
--         ,(Believe ag (Neg $ Prim pr))
--         ,(Neg $ Believe ag (Prim pr))] 
--         | pr <- prims m, ag <- agents m]
tests m = concat [
        [(Know ag (Prim pr))] 
        | pr <- prims m, ag <- agents m]

runT m = do
  let prints = do 
      test    <- tests m
      state   <- states m
      let res = sem m state test
      let padding = (if res then " " else "") ++ "    "
      return $ putStrLn ("[" ++ show res ++ "]" ++padding++ "M," ++ show state ++ " ⊨ " ++ show test)
  sequence prints
  return ()


kripkeToDOTGraph m = "digraph G {" ++ concat (map (\s -> s ++ ";\n") nodesStmts) ++ concat (map (\s -> s ++ ";\n") edgeStmts) ++ "}" 
  where nodesStmts = [ concat (map show $ (states m)!!i) ++ " [label=\"" ++ show i ++ "\"]" | i <- [0..(length (states m))-1]]
        cone (a,b) = concat (map show a) ++ " -> " ++ concat (map show b)
        edgeStmts = [cone ss | ag <- agents m, ss <- accessibility m ag]


remove :: String -> String -> String
remove w "" = ""
remove w s@(c:cs) 
  | w `isPrefixOf` s = remove w (drop (length w) s)
  | otherwise = c : remove w cs
  
kripkeToDOTGraph' m = remove "\\\"" $ "digraph G {" ++ concat (map (\s -> s ++ ";\n") nodesStmts) ++ concat (map (\s -> s ++ ";\n") edgeStmts) ++ "}" 
  where nodesStmts = [ show (show s) ++ " [label=" ++ show (show s) ++ "]" | s <- states m]
        cone (a,b) = show (show a) ++ "  -> " ++ show (show b)
        edgeStmts = [cone ss | ag <- agents m, ss <- accessibility m ag]
