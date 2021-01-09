module Main where

import Control.Applicative

tail' :: [a] -> [a]
tail' (_ : xs) = xs
tail' [] = []

solve :: [Int] -> [[Int]]
solve [] = [[0]]
solve (x : []) = [[x]]
solve (x : xs) = (x :) <$> (take x (iterate tail' xs) >>= solve)

--------------------------------------------------------------------------------

example :: [Int]
example = [6, 2, 4, 0, 5, 1, 1, 4, 2, 9]

less_steps :: [[Int]] -> [Int]
less_steps [] = error "No performed jumps"
less_steps (x : xs) = foldr (\l r -> if length l >= length r then r else l) x xs

main = print . less_steps $ solve example
