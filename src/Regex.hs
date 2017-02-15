module Regex where

data Regex = Zero                   -- empty
            | One                   -- epsilon
            | Lit Char              -- single character
            | Plus Regex Regex      -- union (+)
            | Cat  Regex Regex      -- concatenation (.)
            | Many Regex            -- repetition (*)
            deriving Show

match :: Regex -> String -> Bool
match re str = accept re str null

type Cont = String -> Bool

accept :: Regex -> String -> Cont -> Bool
accept (Lit c) str k = case str of
                         [] -> False
                         (x:xs) -> x == c && k xs
accept Zero str k = False
accept One str k = k str
accept (Cat e1 e2) str k = accept e1 str (\str' -> accept e2 str' k)
accept (Plus e1 e2) str k = accept e1 str k || accept e2 str k
accept (Many e) str k = acceptMany e str k

acceptMany e str k = k str || -- epsilon
  accept e str (\str' -> str' /= str && acceptMany e str' k)
