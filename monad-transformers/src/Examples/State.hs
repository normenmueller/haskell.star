{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Examples.State where

-- cf. All About Monads

import Control.Monad.Trans.State
import System.Random

data MyType =
    MT Int Bool Char Int
    deriving (Show)

makeRandomValue :: StdGen -> (MyType, StdGen)
makeRandomValue g =
    let (n, g1) = randomR (1, 100) g
        (b, g2) = random g1
        (c, g3) = randomR ('a', 'z') g2
        (m, g4) = randomR (-n, n) g3
     in (MT n b c m, g4)

{- Using the State monad, we can define a function that returns a random value
   and updates the random generator state at the same time. -}
getAny :: (Random a) => State StdGen a
getAny = do
    (g :: StdGen) <- get
    (x :: a, g' :: StdGen) <- return $ random g
    put g' :: State StdGen ()
    return x :: State StdGen a

get' :: State StdGen StdGen
get' = state (\s -> (s, s))

-- similar to 'getAny', but it bounds the random value returned
getOne :: (Random a) => (a, a) -> State StdGen a
getOne bounds = do
    (g :: StdGen) <- get
    (x :: a, g' :: StdGen) <- return $ randomR bounds g
    put g' :: State StdGen ()
    return x

{- Using the State monad with StdGen as the state, we can build random complex
   types without manually threading the random generator states through the
   code. -}
makeRandomValueST :: StdGen -> (MyType, StdGen)
makeRandomValueST =
    runState $ do
        n <- getOne (1, 100)
        b <- getAny
        c <- getOne ('a', 'z')
        m <- getOne (-n, n)
        return $ MT n b c m
