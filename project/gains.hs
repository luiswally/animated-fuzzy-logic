module Gains where
import Prelude hiding ((&&), (||), not, and, or, any, all)
import Fuzzy


ffilter :: Fuzzy a -> [a] -> [(a, Double)]
ffilter p xs = filter ((/=) 0 . snd) (map (\x -> (x, p x)) xs)

type Record  = (String, Protein, Weight)
type Weight  = Double -- lbs gained in a week
type Protein = Double -- average daily g of protein consumed in a week

-- general fuzzy subset distribution for skewed triangle
atri :: Double -> Double -> Double -> Fuzzy Double
atri a b c x
  | x < a               = 0.0
  | x < b               = (x - a) / (b - a)
  | x < c               = (c - x) / (c - b)
  | otherwise = 0


protein :: Record -> Protein
protein(_,p,_) = p
weight :: Record -> Weight
weight (_, _, w) = w

weights :: [Weight]
weights = [-2,-1.9..2]

-- fuzzy subsets to be analyzed
proteinIntake :: Fuzzy Protein
proteinIntake = up 50 120
muscleGrowth :: Fuzzy Weight
muscleGrowth = atri 0.25 0.85 1.20


records :: [Record]
records = [("2023-07-12", 75.00, 1.15), ("2023-07-19", 40.65, 0.80),
           ("2023-07-25", 94.20, 1.10), ("2023-08-03", 105.92, 1.00)]

-- combinations of multiple fuzzy subsets to create final fuzzy subset
p1 record = muscleGrowth (weight record)
p2 record = muscleGrowth (weight record) && proteinIntake (protein record)
p3 record = somewhat muscleGrowth (weight record) && very proteinIntake (protein record)