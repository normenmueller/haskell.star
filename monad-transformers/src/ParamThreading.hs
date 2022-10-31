module ParamThreading where

-- The Reader monad, or more generally the MonadReader interface, solves the
-- problem of threading the same configuration to many functions.

-- Imagine this is a directory
type Config = FilePath

load :: Config -> String -> IO String
load config x = readFile (config ++ x)

loadRevision :: Config -> Int -> IO String
loadRevision config x = load config ("history" ++ show x ++ ".txt")
