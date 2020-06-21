{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

-- Source: https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style

import "base" Control.Applicative
import "mtl" Control.Monad.Cont
import "mtl" Control.Monad.State

-- The CoroutineT monad is just ContT stacked with a StateT containing the suspended coroutines.
newtype CoroutineT r m a = CoroutineT { runCoroutineT' :: ContT r (StateT [CoroutineT r m ()] m) a }
    deriving (Functor, Applicative, Monad, MonadCont, MonadIO)

-- Used to manipulate the coroutine queue.
getCCs :: Monad m => CoroutineT r m [CoroutineT r m ()]
getCCs = CoroutineT $ lift get

putCCs :: Monad m => [CoroutineT r m ()] -> CoroutineT r m ()
putCCs = CoroutineT . lift . put

-- Pop and push coroutines to the queue.
dequeue :: Monad m => CoroutineT r m ()
dequeue = getCCs >>= \case
	[] -> return ()
	(p:ps) -> putCCs ps *> p

queue :: Monad m => CoroutineT r m () -> CoroutineT r m ()
queue p = getCCs >>= putCCs . (<> [p])

-- The interface.
yield :: Monad m => CoroutineT r m ()
yield = callCC $ \k -> queue (k ()) *> dequeue

fork :: Monad m => CoroutineT r m () -> CoroutineT r m ()
fork p = callCC $ \k -> queue (k ()) *> p *> dequeue

-- Exhaust passes control to suspended coroutines repeatedly until there isn't any left.
exhaust :: Monad m => CoroutineT r m ()
exhaust = do
    exhausted <- null <$> getCCs
    if not exhausted
        then yield *> exhaust
        else return ()

-- Runs the coroutines in the base monad.
runCoroutineT :: Monad m => CoroutineT r m r -> m r
runCoroutineT = flip evalStateT [] . flip runContT return . runCoroutineT' . (<* exhaust)

printOne n = liftIO (print n) *> yield

main = runCoroutineT $ do
    fork $ replicateM_ 3 (printOne 3)
    fork $ replicateM_ 4 (printOne 4)
    replicateM_ 2 (printOne 2)
