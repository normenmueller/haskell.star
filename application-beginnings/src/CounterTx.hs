module CounterTx where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Util

type Program state config a = StateT state (ReaderT config IO) a

-- Run a `Program` with a given state and config, returning
-- a final value and the final state of the `Program`
runProgram :: Program s c a -> s -> c -> IO (a, s)
runProgram p s = runReaderT (runStateT p s)

counter :: Program CounterState CounterConfig ()
counter = do
    count <- gets currentCount
    countUpTo' <- lift $ asks countUpTo
    unless (count > countUpTo') $ do
        liftIO . putStrLn $ "Current count: " <> show count
        countBy' <- lift $ asks countBy
        modify (\st -> st { currentCount = count + countBy' })
        counter
