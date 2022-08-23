{-# LANGUAGE InstanceSigs #-}

module Parser where

import Data.List (find)
import Data.Maybe (maybeToList,listToMaybe)
import Control.Applicative (Alternative(..))
import Control.Monad (guard)


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

zero :: Parser c a
zero = P (const [])

sat :: (a -> Bool) -> Parser a a
sat p = do { c <- item; if p c then return c else zero }

satStrict :: (a -> Bool) -> Parser a a
satStrict f = do { c <- item; guard (f c); return c }

char :: Char -> Parser Char Char
char c = sat (==c)

first :: Parser c a -> Parser c a
first p = P (\s -> case parse p s of [] -> []; (x:_) -> [x])

alt :: Parser c a -> Parser c a -> Parser c a
alt p q = P (\s -> parse p s ++ parse q s)

-- (<|>) :: Parser c a -> Parser c a -> Parser c a
-- -- p <|> q = alt p q -- p +++ q
-- p <|> q = alt p q

(+++) :: Parser c a -> Parser c a -> Parser c a
p +++ q = first (p `alt` q)
-- (<|>) = (+++) -- alt

instance Alternative (Parser c) where
  empty = zero
  p <|> q = alt p q
  -- p <|> q = P (\s -> case parse (alt p q) s of [] -> []; (x:_) -> [x])
  -- p <|> q = first (alt p q)


seq :: [Parser c a] -> Parser c [a]
seq = sequence

choice :: [Parser c a] -> Parser c a
choice = foldl (<|>) zero

choice1 :: [Parser c a] -> Parser c a
choice1 = first . choice

finished :: Parser c a -> Parser c a
finished p = P (\s -> maybeToList $ find (null . snd) (p <<< s))






introspect :: ([(a,[c])] -> [(b,[c])]) -> Parser c a -> Parser c b
introspect f p = P (\s -> f (p <<< s))

(>->) :: Parser c a -> ([(a, [c])] -> [(b, [c])]) -> Parser c b
p >-> f = introspect f p

source :: Parser a [a]
source = P (\s -> [(s,s)])

peak :: Int -> Parser a [a]
peak n = source >>= return . take n

next :: Parser a (Maybe a)
next = peak 1 >>= return . listToMaybe

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

get :: Parser c a -> Parser c [(a,[c])]
get p = source >>= return . parse p




