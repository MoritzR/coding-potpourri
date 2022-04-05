import Control.Monad (filterM)
import Data.Function (fix)
import Control.Applicative

-- try fibs !! 10000
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibs' :: [Integer]
fibs' = fix $ (0:) . scanl (+) 1

-- e.g. powerset [1,2,3] == [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
powerset :: [b] -> [[b]]
powerset = filterM $ const [True, False]

powerset' :: [b] -> [[b]]
powerset' = filterM (\_ -> pure True <|> pure False)