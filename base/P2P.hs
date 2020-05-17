module Main where

-- Source: https://maxfieldchen.com/posts/2020-05-09-Code-Interview-Haskell-1.html

import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Text       (Text)

type User = Text
type Path = [User]

type Network = M.Map User [User]

shortestPath3 :: User -> User -> Network -> Maybe Path
shortestPath3 from to network = sortOn length <$> dfs to from [from] >>= listToMaybe where

	dfs :: User -> User -> Path -> Maybe [Path]
	dfs to next path = do
		neighbors <- network M.!? next
		case to `elem` neighbors of
			False -> Just $ concat $ mapMaybe
				(\n -> dfs to n (n:path))
				(filter (not . (`elem` path)) neighbors)
			True -> Just [to:path]

network = M.fromList [
	("Jake",     ["Arnold", "Omega", "Frank"]),
	("Arnold",   ["Jake", "Chomsky"]),
	("Omega",    ["Jake", "Taliyah", "San", "Chomsky"]),
	("San",      ["Omega", "Frank"]),
	("Taliyah",  ["Omega", "Larry", "Arragon"]),
	("Larry",    ["Taliyah", "Arragon", "Rengar", "VoidStar"]),
	("Arragon",  ["Taliyah", "Larry", "Liam", "Nathan"]),
	("Chomsky",  ["Nathan", "Omega", "Arnold"]),
	("Frank",    ["San", "Jake", "Scott"])]

main = print $ reverse <$> shortestPath3 "Jake" "Nathan" network
