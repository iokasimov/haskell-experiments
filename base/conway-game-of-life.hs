module Main where

-- Source: https://herebeseaswines.net/essays/2020-10-22-conways-game-of-life-in-haskell

import System.Environment
import Control.Concurrent
import Data.Foldable
import Data.Maybe

type Field = [Cell]
type Cell = (Point, Status)
type Point = (Int, Int)
type Status = Bool

prepare :: [String] -> Field
prepare rawData = concat [line (rawData !! y) y | y <- [0..length rawData - 1]] where

	line :: String -> Int -> [Cell]
	line row y = [((x,y), alive $ row !! x) | x <- [0..length row - 1]]

	alive :: Char -> Bool
	alive 'o' = True
	alive _ = False

representation :: Status -> String
representation True = "[o]"
representation False = "[ ]"

render :: Cell -> IO ()
render ((0, _), status) = putStr $ "\n" <> representation status
render (_, status) = putStr $ representation status

play :: Field -> IO ()
play field = do
	traverse render field
	putStr "\ESC[2J"
	threadDelay 400000
	play $ next field

next :: Field -> Field
next field = makeCell field <$> field

makeCell :: Field -> Cell -> Cell
makeCell field (point, status) = (point, liveliness $ aliveNeighbours field (point, status) directions 0) where

	directions :: [Point]
	directions = [(0,-1), (1,-1), (1,0), (1,1), (0,1), (-1,1), (-1,0), (-1,-1)]

	liveliness :: Integer -> Bool
	liveliness aliveNeighbours
		| aliveNeighbours == 3 && not status = True
		| aliveNeighbours == 2 && status = True
		| aliveNeighbours == 3 && status = True
		| otherwise = False

	aliveNeighbours :: Field -> Cell -> [Point] -> Integer -> Integer
	aliveNeighbours field ((x,y), status) dirs count
		| null dirs = count
		| isAlive (x + fst (head dirs), y + snd (head dirs)) = aliveNeighbours field ((x,y), status) (tail dirs) (count + 1)
		| otherwise = aliveNeighbours field ((x,y), status) (tail dirs) count

	isAlive :: Point -> Bool
	isAlive cell = maybe False snd $ find (condition cell) field

	condition :: Point -> Cell -> Bool
	condition position (point, status) = position == point

start =
	[ "................"
	, "................"
	, ".....o....o....."
	, "...oo.oooo.oo..."
	, ".....o....o....."
	, "................"
	, "................"
	]

main :: IO ()
main = play $ prepare start
