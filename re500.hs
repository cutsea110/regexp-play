module Main where

import RegExpPlay

a = symw ('a'==)
seqn n = foldr1 seqw . replicate n
re n = seqn n (altw a epsw) `seqw` seqn n a

main :: IO ()
main = do
  as <- getContents
  if matchw (re 500) as
    then putStrLn "match"
    else putStrLn "not match"
