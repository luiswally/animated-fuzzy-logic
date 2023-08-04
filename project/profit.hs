module Profit where
import Prelude hiding ((&&), (||), not, and, or, any, all)
import Fuzzy


ffilter :: Fuzzy a -> [a] -> [(a, Double)]
ffilter p xs = filter ((/=) 0 . snd) (map (\x -> (x, p x)) xs)

-- type Percentage = Double
type Sales      = Double -- thousands of pounds
type Company    = (String, Sales, Percentage)



sales :: Company -> Sales
sales(_,s,_) =s
profit :: Company -> Percentage
profit (_, _, p) = p
-- percentages :: [Percentage]
-- percentages = [-10..30]
-- profitable :: Fuzzy Percentage
-- profitable = up 0 15
high :: Fuzzy Sales
high = up 600 1150



companies :: [Company]
companies = [("A", 500, 7),
                 ("B", 600, -9),  ("C", 800, 17),
                 ("D", 850, 12),  ("E", 900, -11), ("F", 1000, 15),
                 ("G", 1100, 14), ("H", 1200, 1),  ("I", 1300, -2),
                 ("J", 1400, -6), ("K", 1500, 12)]


p1 co = profitable (profit co)
p2 co = profitable (profit co) && high (sales co)
p3 co = somewhat profitable (profit co) && very high (sales co)