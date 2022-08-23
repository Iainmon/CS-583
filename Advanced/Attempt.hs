
import Data.Map (Map)
import qualified Data.Map as Map

import Data.List ( sortOn, nub, intercalate)
import Data.Either (isRight, rights)

import Parser
import Grammar

import Debug.Trace


f :: CFG a -> Either NT a -> [[Either NT a]]
f g (Left nt) = prods nt g
f _ (Right c) = [[Right c]]

h :: CFG a -> [Either NT a] -> [[Either NT a]]
h g p = map concat $ map (f g) p

sentences :: CFG a -> [Either NT a] -> [[[Either NT a]]]
sentences g prd = [prd] : map (h g) (concat $ sentences g prd)


concrete :: [Either a b] -> [b]
concrete xs = if all isRight xs then rights xs else []

-- concretes :: CFG a -> [Either NT a] -> [[[Either NT a]]]
concretes :: CFG a -> [Either NT a] -> [[a]]
concretes g = map concrete . concat . sentences g

-- generates :: Eq a => CFG a -> [Either NT a] -> [[a]]
generates :: Eq a => CFG a -> [Either NT a] -> [[a]]
generates g = nub . concretes g
  where go fs [] = []
        go fs (x:xs) = if x `elem` fs then go fs xs else x : go (x:fs) xs
-- generates g = gen [] . concretes g
--   where gen fs (x:xs) | x `elem` fs = gen fs xs
--                       | otherwise   = x : gen (x:fs) xs
--         gen fs _ = []

exn :: CFG a -> Int -> [Either NT a] -> [[Either NT a]]
exn g n p = head $ drop n (sentences g p)


ex :: CFG a -> [Either NT a] -> [[Either NT a]]
ex g w = concat (sentences g w)

concretes' :: CFG a -> [Either NT a] -> [[a]]
concretes' g w = map rights $ filter (and . map isRight) (concat (sentences g w))


run :: CFG Char -> Production Char -> String -> [String]
run _ ss [] = traceM ("[" ++ map (either id id) ss ++ "]\t\t::\t[]\t(done)") >> return (map (either id id) ss)
run _ [Left '*'] _ = [[]]
run g ((Left nt):ss) w = concat [run (rotateProds nt g) ss' w | ss' <- nexts ]
  where nexts = do rule <- prods nt g
                  --  traceShowM (nt,showProd (rule ++ ss))
                   traceM $ map (either id id) (Left nt : ss) ++ "\t\t->\t" ++ map (either id id) (rule ++ ss) ++ "\t" ++ w
                   return $ rule ++ ss
run g ((Right c):ss) (c':w) | c == c'   = run g ss w
                            | otherwise = traceM (map (either id id) (Right c : ss) ++ "\t\t()\t" ++ show c ++ "\t" ++ (c':w)) >> return (map (either id id) ss) -- []
run g ss w = error $ "error!: " ++ (show (ss,w))

rotateProds :: NT -> CFG a -> CFG a
rotateProds = Map.update (return . rotate)

gens :: String -> [String]
gens s = run (shuffleProds 'S' $ rules g4) [Left 'S',Left '*'] s


-- run _ [] _ = [[]]
-- run g ((Left nt):ss) w
--   = do rule <- prods nt g
--        traceShowM (nt,rule ++ ss)
--        run g (rule ++ ss) w
-- run g ((Right c):ss) (c':w) | c == c'   = map ([c] ++) $ run g ss w
--                             | otherwise = []
-- run _ _ _ = []
-- g = makeGrammar (concat rules) 'E'
--   where rules = [ 'E' --> ["N","D","EOE","(E)"]
--                 -- , 'F' --> ["N","D","(E)"]
--                 , 'O' --> ["+","-","*"]
--                 , 'N' --> [return c | c <- ['a'..'z']]
--                 , 'D' --> [show n | n <- [0..9]]
--                 ]

g1 :: Grammar Char
g1 = makeGrammar (concat rules) 'S'
  where rules = [ 'S' --> ["a","SS"] ]

g2 :: Grammar Char
g2 = makeGrammar (concat rules) 'E'
  where rules = [ 'E' --> ["N","D","EOE","(E)"]
                , 'O' --> ["+","-","*"]
                , 'N' --> [return c | c <- ['a'..'z']]
                , 'D' --> [show n | n <- [0..9]]
                ]

g3 :: Grammar Char
g3 = makeGrammar (concat rules) 'S'
  where rules = [ 'S' --> ["c","aS","Sb"] ]

g4 :: Grammar Char
g4 = makeGrammar (concat rules) 'S'
  where rules = [ 'S' --> ["c","aSa","bSb"] ]
