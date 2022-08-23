module Main where

import Parser
import Control.Applicative
import ParseTree
import Grammar



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

-- w2 = "(1*(a+b))"

g3 :: Grammar Char
g3 = makeGrammar (concat rules) 'S'
  where rules = [ 'S' --> ["c","aS","Sb"] ]

g4 :: Grammar Char
g4 = makeGrammar (concat rules) 'S'
  where rules = [ 'S' --> ["c","aSa","bSb"] ]

g5 :: Grammar Char
g5 = makeGrammar (concat rules) 'S'
  where rules = [ 'S' --> ["c","aSaSa","SbS"] ]

main :: IO ()
main = 
  do print $ finished (grammarParser g4) <<< "abcba"
     print $ derive g4 "abcba"
