module Main where

import Crafted
import NDS
import Stack

main :: IO ()
main = do
    print "-- Crafted"
    let withInitialState = 5
    result <- Crafted.runApp app withInitialState
    print result
    print "-- Stack"
    print $ simulateStack [1, 2, 3]
    print $ simulateStack' [1, 2, 3]
    print "-- NDS"
    mapM_ print $ NDS.runApp' nds 0
