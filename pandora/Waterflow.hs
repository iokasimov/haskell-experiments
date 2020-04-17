import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Prelude (Int, print)
import qualified Prelude as P

getmax :: (Applicative t, Stateful a t, Supremum a) => a |-> t
getmax x = modify (x \/) *> current

type Peaksearch t u a = (Traversable t, Applicative u, Pointable u, Stateful a u, Supremum a)

leftmax:: Peaksearch t u a => t a -> u :. t := a
leftmax xs = xs ->> getmax

rightmax :: Peaksearch t u a => t a -> u :. t := a
rightmax xs = run $ xs ->> Backwards . getmax

highest :: (Traversable t, Monoid a) => (State a :. t := a) -> t a
highest = extract . run % zero

--------------------------------------------------------------------------------

instance Semigroup Int where (+) = (P.+)
instance Monoid Int where zero = 0
instance Infimum Int where (/\) = P.min
instance Supremum Int where (\/) = P.max

walls :: Stack Int
walls = push 2 $ push 5 $ push 1 $ push 2 $ push 3 $ push 4 $ push 7 $ push 7 $ push 6 $ empty

main = do
    let (ll :*: lms) = run @(State Int) % 0 . leftmax $ walls
    -- let rms = run % zero . rightmax $ walls
    -- let r = (\l r x -> (l /\ r) P.- x) <$> lms <*> rms <*> walls
    -- let r = (P.+) <$> walls <*> walls :: Stack Int
    lms ->> print
    print ll
    -- print $ (0 \/ 1 :: Int)
    -- print $ attached $ run @(State Int) (modify @Int (P.+ 1)) 0
    -- print "typechecked"
