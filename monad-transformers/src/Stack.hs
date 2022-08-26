-- |Origin at http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/
module Stack where

import Control.Monad.Trans.Identity
import Control.Monad.Trans.State
import Control.Monad.Identity (Identity)

-- push/pop, stack simulation without using State

pop :: [Int] -> (Int, [Int])
pop [] = error "empty stack"
pop (x:xs) = (x, xs)

push :: Int -> [Int] -> ((), [Int])
push x xs = ((), x:xs)

simulateStack :: [Int] -> (Int, [Int])
simulateStack s =
    let (_, s1) = push 3 s
        (x, s2) = pop s1
        (_, s3) = push (x * x) s2
     in pop s3

-- push/pop, stack simulation using State

instance MonadFail Identity where
    fail = error

pop' :: State [Int] Int
pop' = state (\(x:xs) -> (x, xs))
--pop' = do { x:xs <- get; put xs; return x }

push' :: Int -> State [Int] ()
push' x = modify (x:)

simulateStack' :: [Int] -> (Int, [Int])
simulateStack' =
    runState $ do
        push' 3
        x <- pop'
        push' (x * x)
        pop'
