import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Data.Int (Int)
import System.IO (print)

import Gears.Instances

type Maximal = Int

type Rectangles = Prefixed Binary Int Int

type Candidates = Maximal :*: Rectangles

-- TODO Tnis is very inefficient since we need to traverse tree thrice:
-- 1) to find a value, 2) to update it and 3) to update other values
proceed :: Int -> State (Int :*: Rectangles) ()
proceed column =
	void . zoom (access @Rectangles @Candidates) . replace . unite @(->) @(Prefixed _ _)
		=<< zoom (access @Maximal @Candidates) . (update_candidates column <<-) . run
			=<< zoom (access @Rectangles @Candidates) (new_candidate column)

collect :: List Int -> Binary (Int :*: Int)
collect columns = run . extract . attached
	$ run % (0 :*: Prefixed empty) $ proceed <<- columns

-- HERE IS AN ERROR!
-- When we proceed 2 after 3, 2 should aleady contain one cell cause 3 also contans 2 and 1
new_candidate :: Int -> State Rectangles Rectangles
new_candidate column = modify @Rectangles $ \rectangles ->
	lookup @Key column rectangles &
		resolve @Int (rectangles !.) (vary @Element column 0 rectangles)

update_candidates :: Int -> Int :*: Int -> State Int (Int :*: Int)
update_candidates column (height :*: width) = case column <=> height of
	Equal -> point $ height :*: width + 1
	Greater -> point $ height :*: width + 1
	Less -> let area = height * width in
		modify @Int (area \/) $>- (height :*: 0)

example :: List Int
example = item @Push 1 $ item @Push 3 $ item @Push 2 $ item @Push 5 $ empty

main = void $ print <<- collect example
