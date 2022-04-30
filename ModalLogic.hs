

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map


-- type Night = Int
type Person = Int

-- Agents that modal operators can describe
type Ag = Person

-- Primitive statements
data At
  = P Ag   -- `a`'s eyes are blue
  | I Ag   -- `a` lives on the island
  | S Ag   -- `a` will commit suicide
  | S' Ag  -- `a` did not commit suicide (`a` is alive)
  deriving (Show,Eq,Ord)

-- Modal Operators
data Op = K Ag deriving (Show,Eq,Ord)

-- Object language abstract syntax
data Lang
  = Prim At
  | Neg Lang
  | And [Lang]
  | Or [Lang]
  | Imp Lang Lang
  | ModalOp Op Lang
  deriving (Show,Eq,Ord)

-- The truth valuation for a propositional primitive
type TruthAssign = (At,Bool)

-- The set of propositions with their truth valuations,
--   called worlds or states
type State = Map At Bool -- must be Set TruthAssig -- Can have a more blind definition
type State = Set (At,Bool)
type State = Set TruthAssign


-- A function that maps a state and a primative proposition to 
vat :: State -> At -> Bool
vat s a = unjust $ Map.lookup a s
  where unjust (Just a) = a

type Worlds = Set State

-- A function that given an agent, returns a function from a state
--   to the set of states (possible worlds)
type AccessibilityRelation = Ag -> State -> Worlds


-- The type of a function that accepts a state, and returns a 
--   propositional evaluation function for a given primative proposition.
type Valuation = State -> At -> TruthAssign

bielefeld = [1..100]
agents = [1..3]

predicates :: [Ag -> At]
predicates = [P,I,S,S']

cartith :: Int -> [a] -> [[a]]
cartith 0 _  = []
cartith 1 ls = [[x] | x <- ls]
cartith n ls = let subs = cartith (n - 1) ls in [x : xs |  x <- ls, xs <- subs]

-- cartith n ls = [x : concat (cartith (n - 1) ls) | x <- ls]


-- allStates = concatMap concatMap (\a -> concatMap (\pr -> (pr a,b)) predicates) agents
allAssignments = [(pr a,b) | a <- agents, pr <- predicates, b <- [True,False]]
allStates = cartith (length allAssignments) allAssignments -- map (Map.fromList . cartith (length indivAssigns) indivAssigns) 

combs :: [a] -> [[a]]
combs ls = cartith (length ls) ls


considers :: At -> [(At,Bool)]
considers p = [(p,True),(p,False)]

considerOthers a = (S' a,True) : concat [[(P a',True),(S' a',True)] | a' <- agents,a' /= a] -- [(pr a',True) | pr <- [P,S,S'], a' <- agents,a /= a' || pr a /= P a]
considerMyself a = [(P a,True),(P a,False)]
consider a = let cos = considerOthers a in map (:cos) (considerMyself a)

initialStates :: [State]
initialStates = map Map.fromList $ concatMap consider agents


thinksOf :: Ag -> At -> [Bool]
thinksOf a (P a') | a == a'   = [True,False]
                  | otherwise = [True]
thinksOf a (S a') | a == a'   = [False]
                  | otherwise = [True,False]
thinksOf a (S' a') = [True]
thinksOf _ _       = [] 

props = concat [[(P a),(S a),(S' a)] | a <- agents]

considers' :: Ag -> Set (At,Bool)
considers' a = Set.fromList [(p,b) | p <- props, b <- thinksOf a p]



-- initialAssignments = map considers agents -- concatMap (concatMap combs . considers) agents



-- accessible :: Ag -> State -> Worlds
-- accessible a s = concatMap [[(I a',True),(S a,False),]]


-- valuation :: State -> Lang -> Bool
-- valuation s (Prim p)  = vat s p
-- valuation s (Or ps)   = or $ map (valuation s) ps
-- valuation s (And ps)  = and $ map (valuation s) ps
-- valuation s (Imp p q) = valuation s (Or [Not p,q])
-- valuation s (ModalOp (K a) p) = and $ map (flip valuation p) considers
--   where considers = Set.toList $ accessible a s



-- -- Given an agent, returns what the agent knows
-- agentKnowledge :: Ag -> [Truth Lang]
-- agentKnowledge a = Set.toList $ map T $ (Not (ModalOp (K a) (Prim (P a)))):[ModalOp (K a') (Prim (P a')) | a' <- islanders, a /= a']
-- axioms = -- concatMap (\a -> [Imp (ModalOp (K a) (Prim (P a))) (Prim (S a))])
-- states = axioms + ...

-- states = concatMap 
