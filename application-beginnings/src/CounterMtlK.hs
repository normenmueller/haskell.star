{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module CounterMtlK where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Monad.State
import Util

type Program state config a = StateT state (ReaderT config IO) a

-- Run a `Program` with a given state and config, returning
-- a final value and the final state of the `Program`
runProgram :: Program s c a -> s -> c -> IO (a, s)
runProgram p s = runReaderT (runStateT p s)

-- Why 'MonadCounter'?
--
-- Let's assume we would have defined our program as follows:

type Program' state config a = ReaderT config (StateT state IO) a

-- We just flipped 'ReaderT' with 'StateT' on the Tx stack.
-- So, we have to adjust 'runProgram':

runProgram' :: Program' s c a -> s -> c -> IO (a, s)
runProgram' p s c = runStateT (runReaderT p c) s

-- BUT, with 'counter' utilizing 'MonadCounter', we do not need to change the
-- implementation of 'counter'.

-- An interface that describes the effects our program can have in a very
-- general way. I.e. it does not matter how the Tx stack is arranged. 'ReaderT r
-- (StateT ...) a' or 'StateT s (Reader T ...) a'. The general interface just
-- describes the capabilities of the Tx stack, but not the concrete structure!
--
-- Thanks to [Ben Kovach](https://www.kovach.me/) for his explanations!
type MonadCounter m
     = (MonadState CounterState m, MonadReader CounterConfig m, MonadIO m)

counter :: MonadCounter m => m ()
counter = do
    count <- gets currentCount
    countUpTo' <- asks countUpTo
    unless (count > countUpTo') $ do
        liftIO . putStrLn $ "Current count: " <> show count
        countBy' <- asks countBy
        modify (\st -> st { currentCount = count + countBy' })
        counter
