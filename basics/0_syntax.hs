import Data.Function ((&))

{-
significant properties of Haskell:
- pure* (means no statements, for example `something :: a -> ()` is useless)
- strongly typed
- lazy
-}

{-
get the type of a function
Prelude> :t map
map :: (a -> b) -> [a] -> [b]

get documentation on a function
Prelude> :doc print
The 'print' function outputs a value of any printable type ...

get more information on a data type
Prelude> :info Maybe
type Maybe :: * -> *
data Maybe a = Nothing | Just a
        -- Defined in ‘GHC.Maybe’
...
-}

-- define a function
add a b = a + b

--define a function with a type
add2 :: Int -> Int -> Int
add2 a b = a + b

-- if/else
ifElse :: Bool -> a -> a -> a
ifElse bool ifBranch elseBranch =
  if bool
    then ifBranch
    else elseBranch

-- guards (if elif elif else)
somethingWithNumber :: Int -> String
somethingWithNumber number
  | number == 0 = "number is equal to 0"
  | number == 4 = "number is equal to 4"
  | otherwise = "number is something else"

-- let
aPlusBTimesC :: Int -> Int -> Int -> Int
aPlusBTimesC a b c =
  let aPlusB = a + b
   in aPlusB * c

-- where
aPlusBTimesC' :: Int -> Int -> Int -> Int
aPlusBTimesC' a b c = aPlusB + c
  where
    aPlusB = a + b

-- infix notation
plus = add

onePlusTwo = 1 `plus` 2

-- pattern matching
fib 0 = 0
fib 1 = 1
fib n = fib (n -1) + fib (n -2)

orElse :: Maybe a -> a -> a
orElse (Just a) _ = a
orElse Nothing a = a

-- list comprehensions
oneToTen = [1 .. 10]

-- equal to pythons itertools.count(1) which generated an infinite lazy generator
oneToInfinity = [1 ..]

evensFromOneToTen = [2, 4 .. 10]

oneToFive = [i | i <- [1 .. 10], i <= 5]

-- In math this would look like: { i + 1 | i ∈ {1, 2, ..., 10}, i ≤ 5 }
twoToSix = [i + 1 | i <- [1 .. 10], i <= 5]

-- function composition
plus2Times3 = (* 3) . (+ 2)

-- pipeline notation
sumEvens :: [Int] -> Int
sumEvens numbers =
  numbers
    & filter even
    & foldr add 0

-- type holes
-- sumEvens' :: [Int] -> Int
-- sumEvens' numbers =
--     numbers
--         & filter even
--         & foldr _ 0
