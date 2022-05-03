data Parser a = P (String -> [(a,String)])

papply :: Parser a -> String -> [(a,String)]
papply (P p) s = p s



-- instance Monad Parser where
--   return x = P (\s->[(x,s)])
--   P p >>= f = P (\s-> concat [papply (f v) r | (v,r) <- p s])

