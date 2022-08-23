module ParseTree where

import Parser

type NT = Char

data ParseTree a = Rule NT [ParseTree a] | Sym a deriving (Show)


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
        go _ = undefined
-- derivationList tr = (return . same . root) tr : concatMap derivationList (children tr)

parseTrees :: Parser c (ParseTree a) -> [c] -> [ParseTree a]
parseTrees p w = map fst $ filter (null . snd) $ p <<< w
