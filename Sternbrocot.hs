module Sternbrocot (sternbrocot, sternbrocotfraq) where

import Data.Ratio

-- Merges two lists:
-- merge [1, 2, 3] [a, b, c] -> [1, a, 2, b, 3, c]
merge :: [a] -> [a] -> [a]
merge (x:xs) (y:ys) = x:y:(merge xs ys)
merge [] y = y
merge x  _ = x

-- A list of stern-brocot integers: 1, 1, 2, 1, 3, 2...
sternbrocot :: [Int]
sternbrocot = 1:1:merge (zipWith (+) sternbrocot (tail sternbrocot)) (tail sternbrocot)

-- Generates a list of fractions of a sequence:
-- [1, 2, 3, 4] -> [1/2, 2/3, 3/4]
fraqseries :: (Integral a) => [a] -> [Ratio a]
fraqseries (x:y:xs) = (x % y) : (fraqseries (y:xs))

-- Stern-Brocot sequence of rationals, 1/1, 1/2, 2/1, 1/3, 3/2...
sternbrocotfraq :: [Ratio Int]
sternbrocotfraq = fraqseries sternbrocot