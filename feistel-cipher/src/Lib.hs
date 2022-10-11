module Lib where

import Data.Bits

type HashFunction = Int -> [Int] -> [Int]

data KeyGen = KeyGen !(Int -> Int) !Int

feistel :: HashFunction -> Int -> KeyGen -> Bool -> [Int] -> [Int]
feistel f n keyGen decode plain =
    let (l, r) = foldl (pass f) (splitInHalf plain) (keys keyGen n decode)
     in r ++ l

pass :: HashFunction -> ([Int], [Int]) -> Int -> ([Int], [Int])
pass f (l0, r0) k =
    let l1 = r0
        r1 = zipWith xor l0 (f k r0)
     in (l1, r1)

splitInHalf :: [a] -> ([a], [a])
splitInHalf = splitAt =<< (`div` 2) . length

keys :: KeyGen -> Int -> Bool -> [Int]
keys keyGen n decode =
    (if decode
         then reverse
         else id) $
    gen keyGen n

gen :: KeyGen -> Int -> [Int]
gen (KeyGen f seed) len = take len $ iterate f seed
