module Main where

import Stack

main :: IO ()
main = do
    print "-- Stack"
    print $ simulateStack [1, 2, 3]
    print $ simulateStack' [1, 2, 3]
