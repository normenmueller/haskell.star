module Main where

import Lib

main :: IO ()
main =
    let hashFunc x = map (+ x) -- yeah obviously this is a bad hash function
        numOfPasses = 10
        keyGen = KeyGen (* 2) 1 -- and this is not a good keyGen function or seed
        plainText = "A Secret Message"
        blockSize = 32
        paddedPlainText =
            if length plainText > blockSize
                then take blockSize plainText
                else plainText ++ replicate (blockSize - length plainText) ' '
        x0 = map fromEnum paddedPlainText
        x1 = feistel hashFunc numOfPasses keyGen True x0
        x2 = feistel hashFunc numOfPasses keyGen False x1
        x3 = feistel hashFunc numOfPasses keyGen True x2
        x4 = feistel hashFunc numOfPasses keyGen False x3
     in mapM_ print $ plainText : map (map toEnum) [x0, x1, x2, x3, x4]
