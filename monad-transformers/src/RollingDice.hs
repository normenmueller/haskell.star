module RollingDice where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

{-
    System.Random is designed to generate pseudorandom values. You can generate
    those values through providing a seed value or by using the
    system-initialised generator.

    [...]

    Crafted chaining of state---via 'mkStdGen', 'random', 'next'---can get
    tedious.

    Enter the 'State' newtype!
-}
newtype Sum a =
  Sum
    { getSum :: a
    }

{-
    Excursion: newtype

    Newtypes have the same underlying representation as the type they wrap. This
    is because the newtype wrapper disappears at compile time.

    The function (eg. 's -> (a,s)') contained in the newtype must be isomorphic
    to the type (eg. 'State s a') it wraps. That is, there must be a way to go
    from the newtype to the thing it wraps and back again without losing
    information.

    For example, the following demonstrates an isomorphism:
-}
type Iso a b = (a -> b, b -> a)

sumIsIsoWithItsContents :: Iso a (Sum a)
sumIsIsoWithItsContents = (Sum, getSum)

stateIsIsoWithItsContents :: Iso (s -> (a, s)) (State s a)
stateIsIsoWithItsContents = (state, runState)

{-
    Now let us use this kit to generate die such as for a game.
-}
data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

{-
    Now, let's roll the dice.

    This code isn’t optimal, but it does work. It will produce the same results
    every time, because *it is free of effects*, but you can make it produce a
    new result on a new dice roll if you modify the start value.
-}
rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _) = randomR (1, 6) s2
   in (intToDie d1, intToDie d2, intToDie d3)

{-
    So, how can we improve our suboptimal code there? With State, of course!

    Using State will allow us to factor out the generation of a single Die:
-}
rollDie :: State StdGen Die
rollDie =
  let f = do
        (n, s) <- randomR (1, 6)
        return (intToDie n, s)
   in state f

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (sum + die) (count + 1) nextGen
