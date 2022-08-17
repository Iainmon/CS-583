{-# LANGUAGE InstanceSigs #-}



import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (intercalate,find,sortOn, nub)
import Data.Maybe (maybeToList,listToMaybe)

import GHC.Stack
import GHC.Generics
import Debug.Trace
import Unsafe.Coerce (unsafeCoerce)
import Data.Either (isRight, rights)


-- Parser from strings of `c` to things of type `a`
data Parser c a = P ([c] -> [(a,[c])])

parse :: Parser c a -> [c] -> [(a,[c])]
parse (P p) = p

(<<<) = parse

instance Functor (Parser c) where
  fmap f (P p) = P $ \s-> map (\(x,s) -> (f x,s)) (p s)

instance Applicative (Parser c) where
  pure x = P $ \s  -> [(x,s)]
  (<*>) :: Parser c (a -> b) -> Parser c a -> Parser c b
  P f <*> p = P $ \s -> concat [parse (fmap g p) r | (g,r) <- f s]

instance Monad (Parser c) where
  -- return :: a -> Parser c a
  -- return x = P (\s->[(x,s)])
  (>>=) :: Parser c a -> (a -> Parser c b) -> Parser c b
  P p >>= f = P (\s-> concat [parse (f v) r | (v,r) <- p s])

item :: Parser a a
item = P (\s -> if null s then [] else [(head s,tail s)])


source :: Parser a [a]
source = P (\s -> [(s,s)])

peak :: Int -> Parser a [a]
peak n = source >>= return . take n

next :: Parser a (Maybe a)
next = peak 1 >>= return . listToMaybe

introspect :: ([(a,[c])] -> [(b,[c])]) -> Parser c a -> Parser c b
introspect f p = P (\s -> f (p <<< s))

(>->) :: Parser c a -> ([(a, [c])] -> [(b, [c])]) -> Parser c b
p >-> f = introspect f p

freeze :: Parser c a -> Parser c a
freeze p = source >>= (\s -> p >-> map (const' s))
  where const' b (a,_) = (a,b)
-- freeze p = P (\s -> [(v,s) | (v,_) <- p <<< s])

-- Very similar to the parser >>=, but no input is consumed after running p
preParse :: Parser c a -> (a -> Parser c b) -> Parser c b
preParse p = (freeze p>>=)

-- Requires that p parses without fail, then without any input consumed, q is run
(?>) :: Parser c a -> Parser c b -> Parser c b
p ?> q = preParse p (const q)


recognizer :: (a -> Bool) -> Parser c a -> Parser c ()
recognizer f p = do { a <- freeze p; if f a then return () else zero }

filterInputs :: ([c] -> Bool) -> Parser c ()
filterInputs = flip recognizer source

zero :: Parser c a
zero = P (\s -> [])

sat :: (a -> Bool) -> Parser a a
sat p = do { c <- item; if p c then return c else zero }

alt :: Parser c a -> Parser c a -> Parser c a
alt p q = P (\s -> parse p s ++ parse q s)

(+++) :: Parser c a -> Parser c a -> Parser c a
p +++ q = first (p `alt` q)
-- (<|>) = (+++) -- alt

(<|>) :: Parser c a -> Parser c a -> Parser c a
-- p <|> q = alt p q -- p +++ q
p <|> q = alt p q

get :: Parser c a -> Parser c [(a,[c])]
get p = source >>= return . (p<<<)

postParseState :: (Show c, Show a) => String -> Parser c a -> Parser c a
postParseState i p = P (\s -> let rs = p <<< s in let r = take 1 rs in trace ("\t" ++ i ++ ")\t"++ show s ++ " -> " ++ (if null r then "[_]" else show r)) rs)

preParseState :: (Show c, Show a) => String -> Parser c a -> Parser c a
preParseState i p = do { s <- source; v <- p; s' <- source; trace ("  " ++ i ++ "\t==>\t"++ show s ++ "\t->\t" ++ show s' ++ "\t:\t" ++ show v) $ return v }

parseState :: (Show c, Show a) => String -> Parser c a -> Parser c a
parseState = preParseState

finished :: Parser c a -> Parser c a
finished p = P (\s -> maybeToList $ find (null . snd) (p <<< s))

first :: Parser c a -> Parser c a
first p = P (\s -> case p <<< s of [] -> []; (x:xs) -> [x])

char :: Char -> Parser Char Char
char c = sat (==c)

seq :: [Parser c a] -> Parser c [a]
seq = sequence

choice :: [Parser c a] -> Parser c a
choice = foldr (<|>) zero

-- Nonterminals ['A'..'Z']
type NT = Char

-- Production, string of terminals of type `a` or nonterminals
type Production a = [Either NT a]

-- Context Free Grammar over alphabet of type `a`
type CFG a = Map NT [Production a]

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


data ParseTree a = Rule NT [ParseTree a] | Sym a deriving (Show)


-- Generates a parser for terminal symbols
terminal :: (Eq a,Show a) => a -> Parser a (ParseTree a)
-- terminal a | trace ("terminal " ++ show a) False = undefined
terminal a = sat (==a) >>= return . Sym
-- terminal a = sat (==a) >>= return . Sym

-- Eliminates substrings that don't cannot be expressed in the grammar
refine :: (Eq a,Show a) => CFG a -> Parser a ()
refine g = filterInputs condition
  where condition (c:_) = c `elem` initTerms g -- condition = all (flip elem alpha')
        condition _     = False

-- Generates a parser for any derivation of a non-terminal
nonTerminal :: (Eq a,Show a) => CFG a -> NT -> Parser a (ParseTree a)
nonTerminal g nt | trace ("nonTerminal " ++ show nt) False = undefined
nonTerminal g nt = postParseState "ntm" $ parseState (show nt ++ "<-") (refine g' >> (choice (productions g' nt) >>= return . Rule nt))
  where g' = shuffleProds nt g

-- Generates a parser for a production rule
production :: (Eq a,Show a) => CFG a -> Production a -> Parser a [ParseTree a]
-- production g = mapM (symbol g)
production g p | trace ("production " ++ showProd p)  False = undefined
production g p = parseState (showProd p) $ mapM (symbol g) p
-- production g prd = do s <- prd


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

-- tex :: CFG a -> [Either NT a] -> [[Either NT a]]
-- tex g prd = 

showProd :: Show a => Production a -> String
showProd p = filter (/='\'') (concatMap (either show show) p)

-- production g p = introspect (\s -> trace ("parsing production " ++ show p ++ " from " ++ show s ++ " -> " ++ show s) s) $ mapM (symbol g) p
-- production g = sequence . map (symbol g)

-- Generates a parser for each production rule of a non-terminal
productions :: (Eq a,Show a) => CFG a -> NT -> [Parser a [ParseTree a]]
-- productions g nt | trace ("productions " ++ show nt) False = undefined
productions g nt = map (production g) $ prods nt g
-- productions g = map (production g) . flip prods g

-- Generates a parser for a terminal or non-terminal symbol
symbol :: (Eq a,Show a) => CFG a -> Either NT a -> Parser a (ParseTree a)
-- symbol g a | trace ("symbol " ++ show a) False = undefined
-- symbol g (Left nt) = introspect (\s -> trace ("parsing symbol " ++ show nt ++ " from " ++ show s ++ " -> " ++ show s) s) $ nonTerminal g nt -- (shuffleProds nt g) nt 
symbol g (Left nt) = parseState (nt:[]) $ nonTerminal g nt -- (shuffleProds nt g) nt 
-- symbol g (Left nt) = nonTerminal g nt -- (shuffleProds nt g) nt 
symbol g (Right a) = parseState (show a) $ terminal a

-- Generates a parser for rules of a grammar and a non-terminal
makeParser :: (Eq a,Show a) => CFG a -> NT -> Parser a (ParseTree a)
makeParser = nonTerminal


-- The type of a grammar and a specified non-terminal
type Grammar a = (CFG a,NT)

-- Generate a parser, given a pair of a grammar and initial non-terminal
grammarParser :: (Eq a,Show a) => Grammar a -> Parser a (ParseTree a)
grammarParser (g,nt) = makeParser g nt


parseTrees :: Parser c (ParseTree a) -> [c] -> [ParseTree a]
parseTrees p w = map fst $ filter (null . snd) $ p <<< w


derivations :: (Eq a,Show a) => Grammar a -> [a] -> [ParseTree a]
derivations = parseTrees . grammarParser

-- Picks the first derivation that is found
derivations1 :: (Eq a,Show a) => Grammar a -> [a] -> [ParseTree a]
derivations1 = parseTrees . finished . grammarParser


consParser :: (Eq a,Show a) => CFG a -> NT -> Parser a (ParseTree a)
consParser = makeParser


level :: Int -> ParseTree Char -> [Char]
level _ (Sym a)     = [a]
level 0 (Rule nt _) = [nt]
level n (Rule _ cs) = concatMap (level (n-1)) cs

leaves :: ParseTree a -> [a]
leaves (Sym a)     = [a]
leaves (Rule _ cs) = concatMap leaves cs


type Derivation a = [[a]]

derivationList :: ParseTree Char -> Derivation Char
derivationList tr = go $ map (flip level tr) [0..]
  where go (s:ss) | s == leaves tr = [s]
                  | otherwise      = s : go ss


derive :: Grammar Char -> String -> [Derivation Char]
derive g w = map derivationList (derivations g w)

derive1 :: Grammar Char -> String -> [Derivation Char]
derive1 g w = map derivationList (derivations1 g w)

-- derivationList tr = (return . same . root) tr : concatMap derivationList (children tr)


consCFG :: [(NT,String)] -> CFG Char
consCFG g = Map.fromListWith (++) [(nt,return $ map f prd) | (nt,prd) <- g]
  where f c | c `elem` ['A'..'Z'] = Left  c
            | otherwise           = Right c

makeGrammar :: [(NT,String)] -> NT -> Grammar Char
makeGrammar g s = (consCFG g,s)



(-->) :: NT -> [a] -> [(NT,a)]
nt --> []     = []
nt --> (a:as) = (nt,a) : (nt --> as)




g1 = makeGrammar (concat rules) 'S'
  where rules = [ 'S' --> ["a","SS"] ]


g2 = makeGrammar (concat rules) 'E'
  where rules = [ 'E' --> ["N","D","EOE","(E)"]
                , 'O' --> ["+","-","*"]
                , 'N' --> [return c | c <- ['a'..'z']]
                , 'D' --> [show n | n <- [0..9]]
                ]


w2 = "(1*(a+b))"

g3 = makeGrammar (concat rules) 'S'
  where rules = [ 'S' --> ["c","aS","Sb"] ]

g4 = makeGrammar (concat rules) 'S'
  where rules = [ 'S' --> ["c","aSa","bSb"] ]



-- run :: Eq a => CFG a -> [Either NT a] -> [a] -> [b]


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
gens s = run (shuffleProds 'S' $ fst g4) [Left 'S',Left '*'] s


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

-- g1 = consCFG [('S',"aSb"),('S',"ab"),('S',"T"),('T',"aSb")]
-- g1P = makeParser g1 'S'

-- tr = head $ derivations (g1,'S') "aabb"

-- sRule1 = [Right 'a',Left 'S',Left 'b']
-- sRule2 = [Right 'a',Right 'b']

-- g1SRuleP = production g1 sRule2
-- {--
-- ghci> parseProd g1 sRule2 <<< "ab"
-- [([Sym 'a',Sym 'b'],"")]
-- --}

-- {--
-- ghci> prodRule g1 'S' sRule2 <<< "ab"
-- [(Rule 'S' [Sym 'a',Sym 'b'],"")]
-- --}



-- -- palG = consGrammar [ 'S' --> [ c:'S':c:"" | c <- ['a'..'z']] ]
-- -- palP = consParser palG

-- ordLeaves :: ParseTree a -> [a]
-- ordLeaves (Sym a)           = [a]
-- ordLeaves (Rule _ children) = concatMap ordLeaves children

-- pstr = fst . head $ consParser g1 'S' <<< "aabb"

-- -- ppCFG :: Show a => CFG a -> String
-- -- ppCFG g = 
-- --   where ppRule r = (r:"") ++ " -> " ++ intercalate " | " (map ppProd )

-- -- Lazy/infinite representation
-- -- data Gram a = NonTer [Either (Gram a) a]

-- sequence' :: Monad m => [m a] -> m [a]
-- -- sequence' []     = return []
-- sequence' (m:ms) = do a <- m
--                       as <- sequence' ms
--                       return (a:as)

-- g3 = consCFG [('S',"a"),('S',"Sa")]
-- g3P = consParser g3 'S'


