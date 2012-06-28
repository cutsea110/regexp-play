module Main where

import System.Random
import Data.List

randoms' :: RandomGen g => g -> [Char]
randoms' g = (\(x,g') -> x:randoms' g') (randomR ('a','b') g)

genrnd' :: RandomGen g => g -> Int -> Int -> [Char]
genrnd' g w len = f streamR 0 [] []
  where
    streamR = randoms' g
    f :: [Char] -> Int -> [Int] -> [Char] -> [Char]
    f rrs@(r:rs) n ns ys
      | n == len  = reverse ys
      | otherwise = 
        case find (n==) ns of
          Just _  -> f rrs (n+1) ns ('b':ys)
          Nothing -> f rs (n+1) ns' (r:ys)
            where
              ns' = if r=='a' then ((n+w):ns) else ns

genrnd :: RandomGen g => g -> Int -> Int -> [Char]
genrnd g n l = genrnd' g n ((n+1)*(l+1))


main :: IO ()
main = undefined
