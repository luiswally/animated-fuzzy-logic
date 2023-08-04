module Demo where

import Fuzzy 

lucrative :: Fuzzy Percentage
lucrative = up 3 10


-- creating fuzzy type
type Weight = Double -- lbs gained in a week

-- fuzzy subset distribution for skewed triangle
atri :: Double -> Double -> Double -> Fuzzy Double
atri a b c x
  | x < a               = 0.0
  | x < b               = (x - a) / (b - a)
  | x < c               = (c - x) / (c - b)
  | otherwise = 0

muscleGrowth :: Fuzzy Weight
muscleGrowth = atri 0.25 0.85 1.20
