module Main where

import System.Random
import Data.List
import Test.QuickCheck

genAB :: IO Char
genAB = getStdRandom (randomR ('a','b'))

genABs :: Int -> Int -> IO [Char]
genABs w l = g 0 [] []
  where
    g :: Int -> [Int] -> [Char] -> IO [Char]
    g n xs cs | n == l    = return $ reverse cs
              | otherwise = do
                case find (n==) xs of
                  Just _ -> g (n+1) xs ('b':cs)
                  Nothing -> do
                    c <- genAB
                    if c == 'a'
                      then g (n+1) ((n+w):xs) (c:cs)
                      else g (n+1) xs (c:cs)

genrnd :: Int -> Int -> IO [Char]
genrnd w n = genABs w ((w+1)*(n+1))

main :: IO ()
main = undefined
