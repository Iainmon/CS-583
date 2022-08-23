module Debug where

-- import Unsafe.Coerce (unsafeCoerce)
-- import GHC.Stack
-- import GHC.Generics


import Debug.Trace ( trace )

import Parser

postParseState :: (Show c, Show a) => String -> Parser c a -> Parser c a
postParseState i p = P (\s -> let rs = p <<< s in let r = take 1 rs in trace ("\t" ++ i ++ ")\t"++ show s ++ " -> " ++ (if null r then "[_]" else show r)) rs)

preParseState :: (Show c, Show a) => String -> Parser c a -> Parser c a
preParseState i p = do { s <- source; v <- p; s' <- source; trace ("  " ++ i ++ "\t==>\t"++ show s ++ "\t->\t" ++ show s' ++ "\t:\t" ++ show v) $ return v }

parseState :: (Show c, Show a) => String -> Parser c a -> Parser c a
parseState = preParseState

