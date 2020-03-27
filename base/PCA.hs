module Main where

-- Source: https://productivedetour.blogspot.com/2012/12/evaluating-probabilistic-cellular_31.html

import "comonad" Control.Comonad
import "comonad" Control.Comonad.Env
import "streams" Data.Stream.Infinite
import "MonadRandom" Control.Monad.Random

import Prelude hiding (iterate, take, tail, repeat, unzip, zip)

data U x = U (Stream x) x (Stream x) deriving (Functor, Foldable)

right (U a b (c :> cs)) = U (b :> a) c cs
left (U (a :> as) b c) = U as a (b :> c)

instance Traversable U where
	traverse f (U lstream focus rstream) =
		let traversepair f (a,b) = (,) <$> f a <*> f b in
		let pairs = fmap unzip . sequenceA . fmap (traversepair f) $ zip lstream rstream in
		let rebuild c (u,v) = U u c v in
		rebuild <$> f focus <*> pairs

instance Comonad U where
	extract (U _ b _) = b
	duplicate a = U (tail $ iterate left a) a (tail $ iterate right a)

type Probs = (Float,Float,Float,Float)

localRule :: EnvT Probs U Bool -> Rand StdGen Bool
localRule ca =
	let (tt,tf,ft,ff) = ask ca in
	let black prob = (< prob) <$> getRandomR (0,1) in
	case lower ca of
		U (True :> _) _ (True :> _) -> black tt
		U (True :> _) _ (False :> _) -> black tf
		U (False :> _) _ (True :> _) -> black ft
		U (False :> _) _ (False :> _) -> black ff

evolve :: EnvT Probs U Bool -> Rand StdGen (EnvT Probs U Bool)
evolve ca = sequence $ extend localRule ca

history :: StdGen -> EnvT Probs U Bool -> Stream (EnvT Probs U Bool)
history seed initialca =
	-- We need to split the generator because we are lazily evaluating
	-- an infinite random structure. The updated generator never "comes out"!
	-- If we changed runRand for evalRand and tried to reuse the
	-- updated generator for the next iteration, the call would hang.
	let unfoldf (ca,seed) =
		let (seed',seed'') = runRand getSplit seed in
		let nextca = evalRand (evolve ca) seed' in
		(nextca,(nextca,seed''))
	in unfold unfoldf (initialca,seed)

showca :: Int -> U Bool -> String
showca margin ca =
	let char b = if b then '#' else '_' in
	let U left center right = fmap char ca in
	(reverse $ take margin left) <> [center] <> (take margin right)


--------------------------------------------------------------------------------

initial :: EnvT Probs U Bool
initial = EnvT (0.0,0.6,0.7,0.0) $ U (iterate id False) True (iterate id False)

probs :: Probs
probs = ask initial

seed = 177
iterations = 10
margin = 18

main = void . sequence . fmap (putStrLn . showca margin . lower) . take iterations . history (mkStdGen seed) $ initial
