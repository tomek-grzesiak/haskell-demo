module Main where

import Protolude
import Test.QuickCheck

main :: IO ()
main = quickCheck prop_reverse

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs