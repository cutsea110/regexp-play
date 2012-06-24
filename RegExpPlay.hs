module RegExpPlay where

import Prelude hiding (sum)

data Reg = Eps         -- ε
         | Sym Char    -- α
         | Alt Reg Reg -- α|β
         | Seq Reg Reg -- αβ
         | Rep Reg     -- α*

accept :: Reg -> String -> Bool
accept Eps       u = null u
accept (Sym c)   u = u == [c]
accept (Alt p q) u = accept p u || accept q u
accept (Seq p q) u =
  or [accept p u1 && accept q u2|(u1, u2) <- split u]
accept (Rep r)   u =
  or [and [accept r ui | ui <- ps]|ps <- parts u]

split :: [a] -> [([a],[a])]
split []     = [([],[])]
split (c:cs) = ([],c:cs):[(c:s1,s2)|(s1,s2) <- split cs]

parts :: [a] -> [[[a]]]
parts [] = [[]]
parts [c] = [[[c]]]
parts (c:cs) =
  concat [[(c:p):ps,[c]:p:ps]|p:ps <- parts cs]

evencs :: Reg
evencs = Seq (Rep (Seq onec onec)) nocs
  where
    nocs = Rep (Alt (Sym 'a') (Sym 'b'))
    onec = Seq nocs (Sym 'c')

class Semiring s where
  zero, one :: s
  (<+>), (<*>) :: s -> s -> s

data Regw c s = Epsw
              | Symw (c -> s)
              | Altw (Regw c s) (Regw c s)
              | Seqw (Regw c s) (Regw c s)
              | Repw (Regw c s)

sym :: Semiring s => Char -> Regw Char s
sym c = Symw (\x -> if x == c then one else zero)

weighted :: Semiring s => Reg -> Regw Char s
weighted Eps       = Epsw
weighted (Sym c)   = sym c
weighted (Alt p q) = Altw (weighted p) (weighted q)
weighted (Seq p q) = Seqw (weighted p) (weighted q)
weighted (Rep p)   = Repw (weighted p)

acceptw :: Semiring s => Regw c s -> [c] -> s
acceptw Epsw       u = if null u then one else zero
acceptw (Symw f)   u = case u of [c] -> f c;_ -> zero
acceptw (Altw p q) u = acceptw p u <+> acceptw q u
acceptw (Seqw p q) u =
  sum [acceptw p u1 <*> acceptw q u2|(u1,u2) <- split u]
acceptw (Repw r)   u =
  sum [prod [acceptw r ui|ui <- ps]|ps <- parts u]

sum, prod :: Semiring s => [s] -> s
sum  = foldr (<+>) zero
prod = foldr (<*>) one

instance Semiring Bool where
  zero = False
  one = True
  (<+>) = (||)
  (<*>) = (&&)

instance Semiring Int where
  zero = 0
  one = 1
  (<+>) = (+)
  (<*>) = (*)
