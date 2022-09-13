module Main where

import qualified CounterTx as Tx
import qualified CounterMtl as Mtl
import qualified CounterMtlK as MtlK
import Control.Monad
import Util

-- | [Application Beginnings](https://www.kovach.me/Haskell_Application_Beginnings.html)
main :: IO ()
main = do
    config <- getConfig
    -- With transformers and w/ lifting
    void $ Tx.runProgram Tx.counter initialState config
    putStrLn "---"
    -- With MTL and w/o lifting
    void $ Mtl.runProgram Mtl.counter initialState config
    -- XXX void $ Mtl.runProgram' Mtl.counter initialState config
    putStrLn "---"
    -- With MTL and w/ general interface specification via 'ConstraintKinds'
    void $ MtlK.runProgram MtlK.counter initialState config
    void $ MtlK.runProgram' MtlK.counter initialState config
