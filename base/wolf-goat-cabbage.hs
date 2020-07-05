module Main where

-- Source: https://github.com/mstksg/inCode/blob/master/code-samples/monad-plus/WolfGoatCabbage.hs

import Control.Applicative ((<$>), empty)
import Control.Monad (void)

data Character = Man | Wolf | Goat | Cabbage
	deriving (Show, Eq, Enum)

newtype Move = Move Character
	deriving Eq

instance Show Move where
	show (Move Man) = "Man goes alone"
	show (Move Wolf) = "Man carries Wolf"
	show (Move Goat) = "Man carries Goat"
	show (Move Cabbage) = "Man carries Cabbage"

type Journey = [Move]

data Bank = Initial | Far
	deriving (Show, Eq)

check :: (Journey -> Bool) -> Journey -> [Journey]
check checker journey = if checker journey then [journey] else empty

-- | The full journey
solutions :: Int -> [Journey]
solutions n = iterate (>>= step) [[]] !! n >>= check carried

-- | One step of the journey: add a move.
step :: Journey -> [Journey]
step p = do
	next <- Move <$> [Man ..]
	check (legal next) p
	check (not . redundant next) p
	check rules $ next : p

bankFromMovesCount :: Int -> Bank
bankFromMovesCount (even -> True) = Initial
bankFromMovesCount (odd -> True) = Far

-- | Helper functions
bank :: Journey -> Character -> Bank
bank journey character = case character of
	-- By every move farmer is involder because he always carries characters
	Man -> bankFromMovesCount . length $ journey
	-- Count how many times we moved characted and measure the lenght
	character -> bankFromMovesCount . length $ filter (== Move character) journey

-- To move character, Man should be on the same bank with this character
legal :: Move -> Journey -> Bool
legal (Move Man) plan = True
legal (Move character) plan = bank plan character == bank plan Man

redundant :: Move -> Journey -> Bool
redundant move [] = False
redundant move (previous : _) = previous == move

rules :: Journey -> Bool
rules journey
	= bank journey Goat == bank journey Man
	|| bank journey Goat /= bank journey Wolf
	&& bank journey Cabbage /= bank journey Goat

-- Check that all characters transported
carried :: Journey -> Bool
carried journey = all (== Far) $ bank journey <$> [Man .. Cabbage]

-- | Main
main :: IO ()
main = void $ (traverse . traverse) print $ solutions 7
