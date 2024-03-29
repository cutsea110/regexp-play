module RegExpPlay where

import Prelude hiding (sum,seq)
import Data.List (foldl')

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

-- ((a|b)*c(a|b)*c)*(a|b)*
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


data REG = EPS
         | SYM Bool Char
         | ALT REG REG
         | SEQ REG REG
         | REP REG

shift :: Bool -> REG -> Char -> REG
shift _ EPS _ = EPS
shift m (SYM _ x) c = SYM (m && x == c) x
shift m (ALT p q) c = ALT (shift m p c) (shift m q c)
shift m (SEQ p q) c = SEQ (shift m p c) (shift (m && empty p || final p) q c)
shift m (REP r) c = REP (shift (m || final r) r c)

empty :: REG -> Bool
empty EPS = True
empty (SYM _ _) = False
empty (ALT p q) = empty p || empty q
empty (SEQ p q) = empty p && empty q
empty (REP _) = True

final :: REG -> Bool
final EPS = False
final (SYM b _) = b
final (ALT p q) = final p || final q
final (SEQ p q) = final p && empty q || final q
final (REP r) = final r

match :: REG -> String -> Bool
match r [] = empty r
match r (c:cs) = final (foldl' (shift False) (shift True r c) cs)

data REGw c s = REGw { active :: Bool
                     , emptyw :: s
                     , finalw :: s
                     , regw   :: REw c s
                     }
data REw c s = EPSw
             | SYMw (c -> s)
             | ALTw (REGw c s) (REGw c s)
             | SEQw (REGw c s) (REGw c s)
             | REPw (REGw c s)

epsw :: Semiring s => REGw c s
epsw = REGw { active = False
            , emptyw = one
            , finalw = zero
            , regw   = EPSw
            }

symw :: Semiring s => (c -> s) -> REGw c s
symw f = REGw { active = False
              , emptyw = zero
              , finalw = zero
              , regw   = SYMw f
              }

altw :: Semiring s => REGw c s -> REGw c s -> REGw c s
altw p q = REGw { active = active p || active q
                , emptyw = emptyw p <+> emptyw q
                , finalw = finala p <+> finala q
                , regw = ALTw p q
                }
           
seqw :: Semiring s => REGw c s -> REGw c s -> REGw c s
seqw p q = REGw { active = active p || active q
                , emptyw = emptyw p <*> emptyw q
                , finalw = finala p <*> emptyw q <+> finala q
                , regw = SEQw p q
                }

repw :: Semiring s => REGw c s -> REGw c s
repw r = REGw { active = active r
              , emptyw = one
              , finalw = finala r
              , regw = REPw r
              }

finala :: Semiring s => REGw c s -> s
finala r = if active r then finalw r else zero

alt :: Semiring s => REGw c s -> REGw c s -> REGw c s
alt p q = REGw { active = False
               , emptyw = emptyw p <+> emptyw q
               , finalw = zero
               , regw = ALTw p q
               }

seq :: Semiring s => REGw c s -> REGw c s -> REGw c s
seq p q = REGw { active = False
               , emptyw = emptyw p <*> emptyw q
               , finalw = zero
               , regw = SEQw p q
               }

rep :: Semiring s => REGw c s -> REGw c s
rep r = REGw { active = False
             , emptyw = one
             , finalw = zero
             , regw = REPw r
             }

matchw :: (Eq s, Semiring s) => REGw c s -> [c] -> s
matchw r [] = emptyw r
matchw r (c:cs) = finalw (foldl' (shiftw zero) (shiftw one r c) cs)

shiftw :: (Eq s, Semiring s) => s -> REGw c s -> c -> REGw c s
shiftw m r c | active r || m /= zero = stepw m (regw r) c
             | otherwise = r

stepw :: (Eq s, Semiring s) => s -> REw c s -> c -> REGw c s
stepw _ EPSw       _ = epsw
stepw m (SYMw f)   c = let fin = m <*> f c
                       in (symw f) { active = fin /= zero, finalw = fin }
stepw m (ALTw p q) c = altw (shiftw m p c) (shiftw m q c)
stepw m (SEQw p q) c = seqw (shiftw m p c) (shiftw (m <*> emptyw p <+> finalw p) q c)
stepw m (REPw r)   c = repw (shiftw (m <+> finalw r) r c)

submatchw :: (Eq s, Semiring s) => REGw (Int,c) s -> [c] -> s
submatchw r s =
  matchw (seqw arb (seqw r arb)) (zip [0..] s)
  where
    arb = repw (symw (\_ -> one))
    
class Semiring s => Semiringi s where
  index :: Int -> s

symi :: Semiringi s => Char -> REGw (Int,Char) s
symi c = symw weight
  where
    weight (pos,x) | x == c    = index pos
                   | otherwise = zero

data Leftmost = NoLeft  | Leftmost Start
              deriving Show
data Start    = NoStart | Start Int
              deriving Show

instance Semiring Leftmost where
  zero = NoLeft
  one  = Leftmost NoStart
  NoLeft     <+> x      = x
  x          <+> NoLeft = x
  Leftmost x <+> Leftmost y = Leftmost (leftmost x y)
    where
      leftmost NoStart   NoStart   = NoStart
      leftmost NoStart   (Start i) = Start i
      leftmost (Start i) NoStart   = Start i
      leftmost (Start i) (Start j) = Start (min i j)
  NoLeft     <*> _          = NoLeft
  _          <*> NoLeft     = NoLeft
  Leftmost x <*> Leftmost y = Leftmost (start x y)
    where
      start NoStart s = s
      start s       _ = s

instance Semiringi Leftmost where
  index = Leftmost . Start

-- a(a|b)*a
aaba :: Semiringi s => REGw (Int, Char) s
aaba = a `seqw` ab `seqw` a
  where
    a = symi 'a'    
    ab = repw (a `altw` symi 'b')    


data LeftLong = NoLeftLong | LeftLong Range
              deriving Show
data Range    = NoRange    | Range Int Int
              deriving Show

instance Semiring LeftLong where
  zero = NoLeftLong
  one  = LeftLong NoRange
  NoLeftLong <+> x = x
  x          <+> NoLeftLong = x
  LeftLong x <+> LeftLong y = LeftLong (leftlong x y)
    where
      leftlong NoRange     NoRange     = NoRange
      leftlong NoRange     (Range i j) = Range i j
      leftlong (Range i j) NoRange     = Range i j
      leftlong (Range i j) (Range k l)
        | i < k || i == k && j >= l = Range i j
        | otherwise                 = Range k l
  NoLeftLong <*> _ = NoLeftLong
  _          <*> NoLeftLong = NoLeftLong
  LeftLong x <*> LeftLong y = LeftLong (range x y)
    where
      range NoRange     r           = r
      range r           NoRange     = r
      range (Range i _) (Range _ j) = Range i j

instance Semiringi LeftLong where
  index i = LeftLong (Range i i)
