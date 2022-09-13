module Util where

import System.Environment

newtype CounterState =
    CounterState
        { currentCount :: Integer
        }

-- The initial state we're starting with
initialState :: CounterState
initialState = CounterState 0

data CounterConfig =
    CounterConfig
        { countBy :: Integer
        , countUpTo :: Integer
        }

-- Some code to read from our environment variables.
-- Note: This is unsafe, and if either environment variable is
-- a) not set, or
-- b) not formatted like an integer,
-- the program will currently error out.
getConfig :: IO CounterConfig
getConfig = do
    countBy' <- read <$> getEnv "COUNT_BY"
    countUpTo' <- read <$> getEnv "COUNT_UP_TO"
    pure $ CounterConfig countBy' countUpTo'
