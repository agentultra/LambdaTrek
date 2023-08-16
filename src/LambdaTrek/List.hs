module LambdaTrek.List where

allp :: [a -> Bool] -> a -> Bool
allp [] _ = True
allp (p:ps) x = p x && allp ps x
