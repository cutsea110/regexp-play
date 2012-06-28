module Main where

import System.Random
import qualified Data.Map as Map

randoms' :: RandomGen g => g -> [Char]
randoms' g = (\(x,g') -> x:randoms' g') (randomR ('a','b') g)

genrnd' :: RandomGen g => g -> Int -> Int -> [Char]
genrnd' g w len = f streamR 0 Map.empty []
  where
    streamR = randoms' g
    f :: [Char] -> Int -> Map.Map Int a -> [Char] -> [Char]
    f rrs@(r:rs) n ns ys
      | n == len  = reverse ys
      | otherwise = 
        if Map.member n ns
        then f rrs (n+1) ns ('b':ys)
        else f rs (n+1) ns' (r:ys)
            where
              ns' = if r=='a' then Map.insert (n+w) undefined ns else ns

genrnd :: RandomGen g => g -> Int -> Int -> [Char]
genrnd g n l = genrnd' g n ((n+1)*(l+1))


main :: IO ()
main = undefined
