module Score (Score (..)) where

data Score = Score Int | Infinity
  deriving (Eq)

instance Show Score where
  show Infinity = "Score ∞"
  show (Score s) = "Score " ++ show s

instance Ord Score where
  compare Infinity Infinity = EQ
  compare (Score _) Infinity = LT
  compare Infinity (Score _) = GT
  compare (Score a) (Score b) = compare a b

instance Num Score where
  (+) Infinity Infinity = Infinity
  (+) (Score _) Infinity = Infinity
  (+) Infinity (Score _) = Infinity
  (+) (Score a) (Score b) = Score (a + b)

  (-) Infinity Infinity = Infinity
  (-) (Score _) Infinity = Infinity
  (-) Infinity (Score _) = Infinity
  (-) (Score a) (Score b) = Score (a - b)

  (*) Infinity Infinity = Infinity
  (*) (Score _) Infinity = Infinity
  (*) Infinity (Score _) = Infinity
  (*) (Score a) (Score b) = Score (a * b)

  negate Infinity = Infinity
  negate (Score a) = Score (negate a)

  abs Infinity = Infinity
  abs (Score a) = Score (abs a)

  signum Infinity = Infinity
  signum (Score a) = Score (signum a)

  fromInteger a = Score (fromInteger a)
