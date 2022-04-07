import Control.Applicative
  ( Alternative ((<|>)),
    Applicative (liftA2),
  )
import Control.Monad (filterM)
import Data.Function (fix)

-- try fibs !! 10000
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibs' :: [Integer]
fibs' = fix $ (0 :) . scanl (+) 1

-- e.g. powerset [1,2,3] == [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
powerset :: [b] -> [[b]]
powerset = filterM $ const [True, False]

powerset' :: [b] -> [[b]]
powerset' = filterM (\_ -> pure True <|> pure False)

-- Functions as Monads --
-- Q: how to do this pointfree without repeating 'x'?
a :: Integer -> Integer
a x = add (add2 x) (mul2 x)

-- A: with lifting
b :: Integer -> Integer
b = liftA2 add add2 mul2

-- or infix
c :: Integer -> Integer
c = add <$> add2 <*> mul2

add :: Integer -> Integer -> Integer
add = (+)

add2 :: Integer -> Integer
add2 = add 2

mul2 :: Integer -> Integer
mul2 = (* 2)
