{-# LANGUAGE NamedFieldPuns, FlexibleInstances, TupleSections #-}

import Data.List (inits, tails, nub, permutations, nubBy, isPrefixOf, sort)

import Data.Set (Set)
import qualified Data.Set as Set

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

agentsUsed :: Eq ag => L ag at -> [ag]
agentsUsed (Prim _) = []
agentsUsed (Neg phi) = agentsUsed phi
agentsUsed (And p1 p2) = nub $ agentsUsed p1 ++ agentsUsed p2
agentsUsed (Know a p) = nub $ a : agentsUsed p
agentsUsed _ = undefined

primsUsed :: Eq at => L ag at -> [at]
primsUsed (Prim p) = [p]
primsUsed (Neg phi) = primsUsed phi
primsUsed (And p1 p2) = nub $ primsUsed p1 ++ primsUsed p2
primsUsed (Know _ p) = primsUsed p
primsUsed _ = undefined

type Collection a = [a] -- Set a

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



type State prim = ([prim],[prim]) -- (truths, falsehoods)

realWorld :: ([a], [b])
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
consRelations states s = map (map (s,)) $ subsets states

functionify :: Eq a => [(a,b)] -> a -> b
functionify [] _ = undefined
functionify ((x,y):xys) x' | x == x'   = y
                           | otherwise = functionify xys x'




type KripkeStar agent prim = KripkeModel agent prim (State prim)

bijectivePairings :: [a] -> [b] -> [[(a, b)]]
bijectivePairings ags = map (zip ags) . permutations

-- allRelations :: (Eq agent, Ord prim) => [agent] -> [prim] -> [[(agent,[(State prim,State prim)])]]
-- allRelations ags prms = [ [(a,map (re   AalWord) b) | (a,b) <- pairing] | pairing <- bijectivePairings ags prms]

consModels :: (Eq agent, Ord prim) => [agent] -> [prim] -> [KripkeStar agent prim]
consModels ags prms = map (consModel ags prms) relations
  where relations = map functionify $ bijectivePairings ags $ filter ((==) (length ags) . length) $ subsets $ map (realWorld,) states
        states    = consStates prms


findModels :: (Eq ag, Ord at) => L ag at -> [KripkeStar ag at]
findModels formula = filter satisfies models
  where models = consModels (agentsUsed formula) (primsUsed formula)
        satisfies m = sem m realWorld formula


sameRelation :: (Ord ag, Ord at) => KripkeStar ag at -> KripkeStar ag at -> Bool
sameRelation k1 k2 = Set.fromList [(a,s) | a <- agents k1,s <- accessibility k1 a] == Set.fromList [(a,s) | a <- agents k2,s <- accessibility k2 a]


formula = (Know "a" ((Prim "ta") `And` (Prim "tb"))) `And` (Know "b" (Prim "tb"))-- (Know "b" (Neg $ And (Neg $ Prim "ta") (Neg $ Prim "tb")))


allModels = consModels (agentsUsed formula) (primsUsed formula)

foundModels = nubBy sameRelation $ findModels formula


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
        cone (a,b) ag = show (show a) ++ "  -> " ++ show (show b) ++ " [label=" ++ show (show ag) ++ "]"
        edgeStmts = [cone ss ag | ag <- agents m, ss <- accessibility m ag]
main = do mapM_ putStrLn $ map kripkeToDOTGraph' (foundModels)