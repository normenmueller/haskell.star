module CounterMtl where

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

counter :: Program CounterState CounterConfig ()
counter = do
    count <- gets currentCount
    countUpTo' <- asks countUpTo
    unless (count > countUpTo') $ do
        liftIO . putStrLn $ "Current count: " <> show count
        countBy' <- asks countBy
        modify (\st -> st { currentCount = count + countBy' })
        counter

-- Now, let's assume we whould have defined out program as follows:

type Program' state config a = ReaderT config (StateT state IO) a

-- We just flipped 'ReaderT' with 'StateT' on the Tx stack.
-- So, we have to adjust 'runProgram':

runProgram' :: Program' s c a -> s -> c -> IO (a, s)
runProgram' p s c = runStateT (runReaderT p c) s

-- BUT, now 'void $ Mtl.runProgram' Mtl.counter initialState config' does not
-- compile. Cf. 'Main'. Enter 'ConstraintKinds'. Cf. 'CounterMtlK'.
