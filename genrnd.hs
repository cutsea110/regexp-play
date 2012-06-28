module Main where

import System.Environment
import System.Random
import Control.Monad
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BL

randoms' :: RandomGen g => g -> BL.ByteString
randoms' g = (\(c,g') -> c `BL.cons` randoms' g') (randomR ('a','b') g)

genrnd' :: RandomGen g => g -> Int -> Int -> BL.ByteString
genrnd' g w len = f streamR 0 Map.empty BL.empty
  where
    streamR = randoms' g
    f :: BL.ByteString -> Int -> Map.Map Int a -> BL.ByteString -> BL.ByteString
    f rrs n ns ys
      | n == len  = BL.reverse ys
      | otherwise = 
        if Map.member n ns
        then f rrs (n+1) ns' $ 'b' `BL.cons` ys
        else f rs (n+1) ns'' $ r `BL.cons` ys
            where
              r = BL.head rrs
              rs = BL.tail rrs
              ns' = Map.delete n ns
              ns'' = if r=='a' then Map.insert (n+w) undefined ns else ns

genrnd :: RandomGen g => g -> Int -> Int -> String
genrnd g w l = BL.unpack $ genrnd' g w ((w+1)*(l+1))


main :: IO ()
main = do
   w:l:_ <- liftM (map read) getArgs
   g <- getStdGen
   putStrLn $ genrnd g w l
