module Main where

import Crafted

main :: IO ()
main = do
    print "-- Crafted"
    let withInitialState = 5
    result <- Crafted.runApp app withInitialState
    print result

