module Main where

import RegExpPlay

a = symw ('a'==)
arb = symw (const True)
arbs = repw arb
arbn n = seqn n arb
seqn n = foldr1 seqw . replicate n
-- .*a.{20}a.*
dist n = arbs `seqw` a `seqw` arbn 20 `seqw` a `seqw` arbs

main :: IO ()
main = do
  as <- getContents
  if matchw (dist 20) as
    then putStrLn "match"
    else putStrLn "no match"
