{-# LANGUAGE FlexibleContexts #-}
module NDS where

import Control.Monad.State
import Control.Monad.Writer
import Data.Char

-- Place n queens on a chess board so that no queen can attack another.

type S = Int

type W = [String]

-- s -> [(a, s), w]
type Prg a = StateT S (WriterT W []) a

runApp :: Prg Int -> S -> [((Int, S), W)]
runApp p = runWriterT . runStateT p

runApp' :: Prg Int -> S -> [(Int, W)]
runApp' p = runWriterT . evalStateT p

-- perform a series of computations in the combined monad, lifting computations
-- from other monads as necessary.
nds :: Prg Int
nds = do
    x <- lift $ getLogLength $ logString "hello"
    addDigits x
    x <- lift $ logEach [1, 3, 5]
    lift $ logVal x
    lift . lift $ getDigits 287

{- Here is a computation on lists -}

-- return the digits of a number as a list
getDigits :: Int -> [Int]
getDigits n =
    let s = show n
     in digitToInt <$> s

{- Here are some computations in 'MonadWriter' -}

-- write a value to a log an return that value
logVal :: (MonadWriter W m) => Int -> m Int
logVal n = do
    tell ["logVal: " ++ show n]
    return n

-- do a logging computation and return the length of the log it wrote
getLogLength :: (MonadWriter [[a]] m) => m b -> m Int
getLogLength c = do
    (_, l) <- listen c
    return (length (concat l))

-- log a string value and retrun 0
logString :: (MonadWriter [String] m) => String -> m Int
logString s = do
    tell ["logString: " ++ s]
    return 0

{- Here is a computation that requires a WriterT [String] [] -}

-- "Fork" the computation and log each list item in a different branch
logEach :: Show a => [a] -> WriterT [String] [] a
logEach xs = do
    x <- lift xs
    tell ["logEach: " ++ show x]
    return x

{- Here is a computation in MonadState -}

-- increment the state by a specified value
addVal :: (MonadState Int m) => Int -> m ()
addVal n = do
    x <- get
    put (x + n)

{- Here are some computations in the combined monad -}

-- set the state to a given value, and log that value
setVal :: Int -> Prg ()
setVal n = do
    x <- lift $ logVal n
    put x

-- "Fork" the computation, adding a different digit to the state in each branch.
-- Because 'setVal' ist used, the new values are logged as well.
addDigits :: Int -> Prg ()
addDigits n = do
    x <- get
    y <- lift . lift $ getDigits n
    setVal (x + y)
