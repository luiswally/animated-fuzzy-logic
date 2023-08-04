module Fuzzy where

-- -- 2 Fuzzy logic
-- Basic operators defined for fuzzy logic application
import Prelude hiding ((&&), (||), not, and, or , any, all)

-- Fuzzy variable class allows shadowing of standard operators for fuzzy variables ONLY
class Logic a where
    true, false :: a
    (&&), (||)  :: a -> a -> a
    not         :: a -> a

-- overloading standard operators to work on fuzzy instances of Logic class
and, or :: Logic a => [a] -> a
and      = foldr (&&) true
or       = foldr (||) false

any, all :: Logic b => (a -> b) -> [a] -> b
any p     = or . map p
all p     = and . map p

-- fuzzy instances of Logic class
instance Logic Double where
    true     = 1
    false    = 0
    (&&)     = min
    (||)     = max
    not x    = 1 - x

-- rebuilt using descriptions provided in research paper
instance Logic Bool where
    true       = True
    false      = False
    True && b  = b
    False && _ = False
    True || _  = True
    False || b = b
    not True   = False
    not False  = True

-- 3 Fuzzy subsets
type Fuzzy a = a -> Double

up :: Double -> Double -> Fuzzy Double
up a b x
  | x < a     = 0.0
  | x < b     = (x - a) / (b - a)
  | otherwise = 1.0

-- tri custom
tri :: Double -> Double -> Fuzzy Double
tri a b x
  | x < a               = 0.0
  | x < (a + b) / 2     = 2 * (x - a) / (b - a)
  | x < b               = 2 * (b - x) / (b - a)
  | otherwise = 0

-- trap custom
trap :: Double -> Double -> Double -> Double -> Fuzzy Double
trap a b c d x
  | x < a               = 0.0
  | x < b               = (x - a) / (b - a)
  | x < c               = 1.0
  | x < d               = (d - x) / (d - c)
  | otherwise = 0

type Percentage = Double

profitable :: Fuzzy Percentage
profitable = up 0 15

-- 3.1 Domain support and fuzzines of fuzyz subset
type Domain a = [a]
supp :: Domain a -> Fuzzy a -> [a]
supp dom f = filter (\x -> f x > 0) dom

fuzziness :: Domain a -> Fuzzy a -> Double
fuzziness dom f = (2.0 / size_dom) * sum (map (delta.f) dom)
  where
    size_dom = fromIntegral (length dom)
    delta x
      | x < 0.5   = x
      | otherwise = 1.0 - x


-- 3.2 Fuzzy subset operations
-- standard set operations
instance (Logic b) => Logic (a -> b) where
    true        = \x -> true       -- everything
    false       = \x -> false      -- empty
    f && g      = \x -> f x && g x -- intersection
    f || g      = \x -> f x || g x -- union
    not f       = \x -> not (f x)  -- complement

-- standard set operations
instance (Num a, Num b) => Num (a, b) where
instance (Num b) => Num (a -> b) where
    f + g           = \x -> f x + g x    
    f * g           = \x -> f x * g x    
    abs f           = \x -> abs (f x)
    signum f        = \x -> signum (f x)
    negate f        = \x -> negate (f x)
    fromInteger i   = \x -> fromInteger i

-- tuple set operations
instance (Logic a, Logic b) => Logic (a, b) where
    true                 = (true, true)
    false                = (false, false)
    (a, b) && (a', b')   = (a && a', b && b')
    (a, b) || (a', b')   = (a || a', b || b')
    not (a, b)           = (not a, not b)

-- 3.3 Hedges and fuzzy numbers
hedge :: Double -> Fuzzy a -> Fuzzy a
hedge p f x = if fx == 0 then 0 else fx ** p
  where fx = f x

very, extremely, somewhat, slightly :: Fuzzy a -> Fuzzy a
very           = hedge 2
extremely      = hedge 3
somewhat       = hedge 0.5
slightly       = hedge (1 / 3)

approximate :: Double -> Double -> Domain Double -> Fuzzy Double
approximate fuzziness n dom = tri (n - hw) (n + hw)
  where hw = fuzziness * (maximum dom - minimum dom)

near, around, roughly :: Double -> Domain Double -> Fuzzy Double
near    = approximate 0.125
around  = approximate 0.25
roughly = approximate 0.375


-- 3.4 Defuzziifcation
centroid :: Domain Double -> Fuzzy Double -> Double
centroid dom f = (sum (zipWith (*) dom fdom)) / (sum fdom)
  where fdom = map f dom


maxima :: Ord a => Domain a -> Fuzzy a -> [a]
maxima dom f = maxima' dom []
  where
    maxima' [] ms         = ms
    maxima' (x:xs) []     = maxima' xs [x]
    maxima' (x:xs) (m:ms)
       | f x > f m        = maxima' xs [x]
       | f x == f m       = maxima' xs (x:m:ms)
       | otherwise        = maxima' xs (m:ms)

minmax, medmax, maxmax :: Ord a => Domain a -> Fuzzy a -> a
minmax dom f = minimum (maxima dom f)
maxmax dom f = maximum (maxima dom f)
medmax dom f = median (maxima dom f)
  where
  median ms    = head (drop (length ms `div` 2) (qsort ms))
  qsort []     = []
  qsort (x:xs) = qsort [y | y <- xs, y <= x] ++ [x] ++
                 qsort [y | y <- xs, y > x]

