module Regex where

data Regex = Zero                   -- empty
            | One                   -- epsilon
            | Lit Char              -- single character
            | Plus Regex Regex      -- union (+)
            | Cat  Regex Regex      -- concatenation (.)
            | Many Regex            -- repetition (*)
            deriving Show

match :: Regex -> String -> Bool
match = undefined

type Cont = String -> Bool

accept :: Regex -> String -> Cont -> Bool
accept = undefined
