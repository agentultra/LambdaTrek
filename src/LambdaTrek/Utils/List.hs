module LambdaTrek.Utils.List where

import Data.List

-- | Update several items in a list at specified indices.
--
-- Duplicate indices will only use the first `find` comes cross in
-- @assocs@.
(//) :: [a] -> [(Int, a)] -> [a]
(//) = go 0 []
  where
    go :: Int -> [a] -> [a] -> [(Int, a)] -> [a]
    go _ acc [] _ = acc
    go idx acc (x:xs) updates = case find ((== idx) . fst) updates of
      Nothing -> go (idx + 1) (x:acc) xs updates
      Just (_, y) -> go (idx + 1) (y:acc) xs updates
