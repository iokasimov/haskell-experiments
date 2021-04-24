{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

-- Source: http://www.randomhacks.net.s3-website-us-east-1.amazonaws.com/2007/02/21/refactoring-probability-distributions/

import "base" Control.Monad (liftM)
import "base" Data.Foldable (foldl')
import "base" Data.List (group, sort)
import "random" System.Random (getStdRandom, random)
import "transformers" Control.Monad.Trans.Class (MonadTrans (lift))
import "transformers" Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))

newtype Prob = Prob Float deriving (Eq, Ord, Num, Fractional)

instance Show Prob where
	show (Prob p) = show intPart ++ "." ++ show fracPart ++ "%" where

		digits = round (1000 * p)
		intPart = digits `div` 10
		fracPart = digits `mod` 10

data Perhaps a = Perhaps a Prob deriving Show

neverHappens (Perhaps _ 0) = True
neverHappens _ = False

instance Functor Perhaps where
	fmap f (Perhaps x p) = Perhaps (f x) p

instance Applicative Perhaps where
	Perhaps f _ <*> Perhaps x p = Perhaps (f x) p

instance Monad Perhaps where
	return x = Perhaps x 1
	ph >>= f = if neverHappens ph then never else Perhaps x (p1 * p2) where

		(Perhaps (Perhaps x p1) p2) = f <$> ph

class Monad m => MonadPerhaps m where
	perhaps :: a -> Prob -> m a
	never :: m a

instance MonadPerhaps Perhaps where
	never = Perhaps undefined 0
	perhaps = Perhaps

newtype PerhapsT m a = PerhapsT { runPerhapsT :: m (Perhaps a) }

instance MonadTrans PerhapsT where
	lift = PerhapsT . liftM return

instance Monad m => Functor (PerhapsT m) where
	fmap = liftM

instance Monad m => Applicative (PerhapsT m) where
	pure = return
	f <*> x = undefined

instance Monad m => Monad (PerhapsT m) where
	return = lift . return
	m >>= f = PerhapsT bound where

		bound = do
			ph <- runPerhapsT m
			case ph of
				Perhaps x1 0 -> return never
				Perhaps x1 p1 -> do
					Perhaps x2 p2 <- runPerhapsT $ f x1
					return . Perhaps x2 $ p1 * p2

-- type Dist = PerhapsT ([])

-- uniform = weighted . map (, 1)

-- weighted :: [(a, Float)] -> FDist a
-- weighted [] = error "Empty probability distributuion"
-- weighted xws = PerhapsT (weight <$> xws) where
--
-- 	weight (x,w) = Perhaps x $ Prob (w / sum)
-- 	sum = foldl' (\w1 (_,w2) -> w1+w2) 0 xws

--------------------------------------------------------------------------------

-- Source: http://www.randomhacks.net.s3-website-us-east-1.amazonaws.com/2007/02/21/randomly-sampled-distributions/

type Weight = Float

class (Functor d, Monad d) => Dist d where
	weighted :: [(a, Weight)] -> d a

-- instance Dist FDist where
-- 	weighted :: [(a, Float)] -> FDist a
-- 	weighted [] = error "Empty probability distributuion"
-- 	weighted xws = PerhapsT (weight <$> xws) where
--
-- 		weight (x,w) = Perhaps x $ Prob (w / sum)
-- 		sum = foldl' (\w1 (_,w2) -> w1+w2) 0 xws

uniform :: Dist d => [a] -> d a
uniform = weighted . map (,1)

data Child = Girl | Boy deriving (Show, Eq, Ord)

child :: Dist d => d Child
child = uniform [Girl, Boy]

family :: Dist d => d [Child]
family = do
	x <- child
	y <- child
	return $ [x, y]

-- family = (\x y -> [x,y]) <$> child <*> child

type FDist = PerhapsT ([])

instance Dist FDist where
	weighted [] = error "Empty distribution"
	weighted xws = PerhapsT (map weight xws) where

		weight (x,w) = Perhaps x (Prob (w / sum))
		sum = foldl' (+) 0 $ snd <$> xws

exact :: FDist a -> [Perhaps a]
exact = runPerhapsT

newtype Rand a = Rand { runRand :: IO a }

randomFloat :: Rand Float
randomFloat = Rand $ getStdRandom random

instance Functor Rand where
	fmap = liftM

instance Applicative Rand where
	pure = Rand . return
	Rand f <*> Rand x = Rand $ f <*> x

instance Monad Rand where
	return = Rand . return
	r >>= f = Rand $ runRand r >>= runRand . f

instance Dist Rand where
	weighted = liftF . weighted

liftF :: FDist a -> Rand a
liftF fdist = randomFloat >>= flip pick (runPerhapsT fdist) . Prob

pick :: Monad m => Prob -> [Perhaps a] -> m a
pick _ [] = error "No values to pick from"
pick n (Perhaps x p : ps) = if n <= p then return x else pick (n - p) ps

sample :: Rand a -> Int -> Rand [a]
sample r n = sequence $ replicate n r

sampleIO r n = runRand $ sample r n

histogram :: Ord a => [a] -> [Int]
histogram = map length . group . sort

--------------------------------------------------------------------------------

data Test = Pos | Neg deriving (Show, Eq)

data HeroinStatus = User | Clean deriving (Show, Eq)

drugTest1 :: Dist d => d (HeroinStatus, Test)
drugTest1 = do
	heroinStatus <- percentUser 0.1
	testResult <- if heroinStatus == User then percentPos 99 else percentPos 1
	return (heroinStatus, testResult)

-- Some handy distributions.
percentUser p = percent p User Clean
percentPos p = percent p Pos Neg

-- A weighted distribution with two elements.
percent p x1 x2 = weighted [(x1, p), (x2, 100 - p)]

drugTest2 :: Dist d => d (Maybe HeroinStatus)
drugTest2 = uncurry decide <$> drugTest1 where

	decide status result = if result == Pos then Just status else Nothing

value (Perhaps x _) = x
prob (Perhaps _ p) = p

type FDist' = MaybeT FDist

instance Dist FDist' where
	weighted xws = lift $ weighted xws

bayes :: FDist' a -> [Perhaps a]
bayes = exact . onlyJust . runMaybeT where

	onlyJust :: FDist (Maybe a) -> FDist a
	onlyJust dist = if total > 0 then PerhapsT $ adjust <$> filtered else PerhapsT [] where

		filtered = ignoring $ runPerhapsT dist
		total = sum $ prob <$> filtered
		adjust (Perhaps x p) = Perhaps x $ p / total

		ignoring :: [Perhaps (Maybe a)] -> [Perhaps a]
		ignoring [] = []
		ignoring (Perhaps Nothing _ : xs) = ignoring xs
		ignoring (Perhaps (Just x) p : xs) = Perhaps x p : ignoring xs

condition :: Bool -> FDist' ()
condition = MaybeT . return . toMaybe where

	toMaybe :: Bool -> Maybe ()
	toMaybe True  = Just ()
	toMaybe False = Nothing

drugTest3 :: FDist' HeroinStatus -> FDist' HeroinStatus
drugTest3 prior = do
	heroinStatus <- prior
	testResult <- if heroinStatus == User then percentPos 99 else percentPos 1
	condition $ testResult == Pos
	return heroinStatus

main = do
	print $ exact family
	-- print $ exact drugTest1
	-- traverse print $ exact drugTest2
	-- traverse print . exact $ onlyJust drugTest2
	traverse print . bayes . drugTest3 $ percentUser 0.1
	print "--------------------------------------------"
	traverse print . bayes . drugTest3 $ percentUser 50
	-- liftM histogram $ sampleIO family 1000
