module Main where

-- Source: https://kwannoel.xyz/229a55bd.html

import Prelude hiding (reverse)

reverse :: [a] -> [a]
reverse = run ($ []) where

	run :: (([a] -> [a]) -> [a]) -> [a] -> [a]
	run k [] = k id
	run k (x : xs) = run ($ k (x :)) xs

main :: IO ()
main = print $ reverse ([1..1000000] :: [Int])
