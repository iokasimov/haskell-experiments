{-# LANGUAGE UndecidableInstances #-}

import "base" Data.Int (Int)
import "base" Data.List ((++))
import "base" System.IO (print)
import "base" Text.Show (show)
import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Gears.Instances ()
import Gears.Utils (show_zipper_list)

import Debug.Trace (traceShow)

validate :: Zipper List Int -> Conclusion (Zipper List Int) (Zipper List Int)
validate zipper = resolve @(Zipper List Int) failure (point zipper) . (|- pyramid) . (side <-> side) . run $ lower zipper where

	pyramid :: Maybe (List Int) -> Maybe (List Int) -> Maybe (Zipper List Int)
	pyramid ls rs = Tap (extract zipper) <$> (twosome <$> ls <*> rs)

	side :: List Int -> Maybe (List Int)
	side xs = extract <$> run (xs ->> slide) (extract zipper)

	slide :: Int -> State Int :> Maybe := Int
	slide now = current @Int >>= \before ->
		before - now == 1 ? replace @Int now $ nothing

type Enumeration = Comprehension Maybe

instance (forall a . Semigroup (t <:.> Construction t := a), Avoidable t, Monad t) => Monad (Comprehension t) where

instance Nullable (Comprehension Maybe) where
	null = Predicate $ \case { Comprehension (TU Nothing) -> True ; _ -> False }

explore :: Zipper List Int -> Conclusion (Zipper List Int) :. Zipper List :. Zipper List := Int
explore bricks = chipped <$> sequence (bricks =>> validate) where

	chipped :: Zipper List (Zipper List Int) -> Zipper List (Zipper List Int)
	chipped shifted = over (focus @Head) decrement <$> shifted

	decrement :: (Setoid a, Quasiring a, Group a) => a -> a
	decrement x = x == zero ? zero $ x - one

nonempty_list_to_zipper_list :: Nonempty List ~> Zipper List
nonempty_list_to_zipper_list xs = Tap (extract xs) $ twosome / unite (deconstruct xs) / empty

zipper_list_to_list :: Zipper List ~> List
zipper_list_to_list (Tap x (T_U (bs :*: fs))) =
	attached . run @(State _) % item @Push x bs
		$ fs ->> modify @(List _) . item @Push

wrong_example, valid_example :: Nonempty List Int
wrong_example = item @Push 0 $ item @Push 1 $ item @Push 5
	$ item @Push 3 $ item @Push 2 $ item @Push 1 $ point 0
valid_example = item @Push 0 $ item @Push 1 $ item @Push 2
	$ item @Push 3 $ item @Push 2 $ item @Push 1 $ point 0

main = void $ do
	print "..................."
	let wrong = nonempty_list_to_zipper_list wrong_example
	print $ (explore wrong >>= (->> explore) >>= (->>> explore) >>= (->>>> explore))
