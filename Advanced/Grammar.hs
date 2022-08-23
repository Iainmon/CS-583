{-# LANGUAGE TypeSynonymInstances #-}

module Grammar where


import Data.Map (Map)
import qualified Data.Map as Map

import Data.List ( sortOn, nub, intercalate)
import Data.Either (isRight, rights)

import Parser
import ParseTree hiding (NT)

import qualified Data.Bifunctor as BF
import Control.Monad (forM, guard)
import Data.Functor ( ($>) )
import Control.Applicative (Alternative(some,many))

import Debug.Trace

-- Nonterminals ['A'..'Z']
type NT = Char

-- Production, string of terminals of type `a` or nonterminals
type Production a = [Either NT a]

showProd :: Show a => Production a -> String
showProd p = filter (/='\'') (concatMap (either show show) p)


-- Context Free Grammar over alphabet of type `a`
type CFG a = Map NT [Production a]

consCFG :: [(NT,String)] -> CFG Char
consCFG g = Map.fromListWith (++) [(nt,return $ map f prd) | (nt,prd) <- g]
  where f c | c `elem` ['A'..'Z'] = Left  c
            | otherwise           = Right c

(-->) :: NT -> [a] -> [(NT,a)]
nt --> []     = []
nt --> (a:as) = (nt,a) : (nt --> as)


showCFG :: Show a => CFG a -> String
showCFG g = unlines [nt : " -> " ++ intercalate " | " (map showProd $ prods nt g) | nt <- nonTerms g]


nonTerms :: CFG a -> [NT]
nonTerms = Map.keys

prods :: NT -> CFG a -> [Production a]
prods = Map.findWithDefault []

allProds :: CFG a -> [Production a]
allProds = concat . Map.elems

prodTerms :: Production a -> [a]
prodTerms = concatMap right
  where right (Left _)  = []
        right (Right a) = [a]

alphabet :: CFG a -> [a]
alphabet = concatMap prodTerms . allProds

-- initTerms :: CFG a -> [a]
initTerms :: CFG a -> [a]
initTerms = concatMap firstR . allProds
  where firstR ((Right a):_) = [a]
        firstR _             = []

rotate :: [a] -> [a]
rotate []     = []
rotate (x:xs) = xs ++ [x]


prioritize :: [Production a] -> [Production a]
prioritize ps = sortOn metric ps
  where metric p = metric' [] p
          where metric' _ []       = 0
                metric' p' (x:xs)  = (proj x * (length ps - length p')) + metric' (x:p') xs
        proj (Left _)  = 1
        proj (Right _) = 0


shuffleProds :: NT -> CFG a -> CFG a
shuffleProds = Map.update (return . prioritize)





-- Generates a parser for terminal symbols
terminal :: (Show a,Eq a) => a -> Parser a (ParseTree a)
-- terminal a = sat (==a) >>= return . Sym
terminal a = satStrict (==a) >>= return . Sym

-- Eliminates substrings that don't cannot be expressed in the grammar
refine :: (Show a,Eq a) => CFG a -> Parser a ()
refine g = freeze $ filterInputs condition
  where condition (c:_) = c `elem` initTerms g && c `elem` alphabet g -- condition = all (flip elem alpha')
        condition _     = False

-- Generates a parser for any derivation of a non-terminal
nonTerminal :: (Show a,Eq a) => CFG a -> NT -> Parser a (ParseTree a)
-- nonTerminal g nt = refine g' >> choice (productions g' nt) >>= return . Rule nt
--   where g' = shuffleProds nt g
-- nonTerminal g nt = refine g' >> (Rule nt <$> choice (productions g' nt))
--   where g' = shuffleProds nt g
nonTerminal g nt = refine g >> (Rule nt <$> choice (productions g' nt))
  where g' = shuffleProds nt g

-- seq' ps = 

-- Generates a parser for a production rule
production :: (Show a,Eq a) => CFG a -> Production a -> Parser a [ParseTree a]
production g = sequence . map (symbol g)
-- production g [] = return []
-- production g (p:ps) = ((:) <$> symbol g p <*> production g ps)

-- production g p = mapM (symbol g) p
-- Generates a parser for each production rule of a non-terminal
productions :: (Show a,Eq a) => CFG a -> NT -> [Parser a [ParseTree a]]
productions g = map (production g) . flip prods g

-- Generates a parser for a terminal or non-terminal symbol
symbol :: (Show a,Eq a) => CFG a -> Either NT a -> Parser a (ParseTree a)
symbol g (Left nt) =  nonTerminal g nt -- (shuffleProds nt g) nt 
symbol g (Right a) =  terminal a

-- Generates a parser for rules of a grammar and a non-terminal
makeParser :: (Show a,Eq a) => CFG a -> NT -> Parser a (ParseTree a)
makeParser = nonTerminal




-- The type of a grammar and a specified non-terminal
newtype Grammar a = G (CFG a,NT)

rules :: Grammar a -> CFG a
rules (G (g,_)) = g

start :: Grammar a -> NT
start (G (_,s)) = s

makeGrammar :: [(NT,String)] -> NT -> Grammar Char
makeGrammar g s = G (consCFG g,s)

grammarFromCFG :: CFG a -> NT -> Grammar a
grammarFromCFG g nt = G (g,nt)

instance Functor Grammar where
  fmap f (G (g,s)) = G (g',s)
    where g' = fmap (fmap $ fmap (BF.second f)) g

instance Show a => Show (Grammar a) where
  show (G (g,s)) = "-> " ++ [s] ++ "\n" ++ unlines (map ("   "++) $ lines $ showCFG g)




-- Generate a parser, given a pair of a grammar and initial non-terminal
grammarParser :: (Show a,Eq a) => Grammar a -> Parser a (ParseTree a)
grammarParser g = makeParser (rules g) (start g)


derivations :: (Show a,Eq a) => Grammar a -> [a] -> [ParseTree a]
derivations = parseTrees . grammarParser

-- Picks the first derivation that is found
derivations1 :: (Show a,Eq a) => Grammar a -> [a] -> [ParseTree a]
derivations1 = parseTrees . finished . grammarParser


derive :: Grammar Char -> String -> [Derivation Char]
derive g w = map derivationList (derivations g w)

derive1 :: Grammar Char -> String -> [Derivation Char]
derive1 g w = map derivationList (derivations1 g w)







