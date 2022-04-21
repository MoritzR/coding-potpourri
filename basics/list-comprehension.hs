import Control.Monad (guard)

-- a function posing as an expensive computation
expensive :: Int -> Int
expensive = (* 10)

-- doing an expensive documentation, then do a check and either continue with the calculated value or discard it
-- it is something like a map -> filter -> map in one step
a :: [Int]
a = [1, 2, 3] >>= (\e -> let e' = expensive e in if even e' then [e' + 1] else [])

-- apply suggested transformation to list comprehension
b, b' :: [Int]
b = [1, 2, 3] >>= (\e -> let e' = expensive e in [e' + 1 | even e'])
-- move let inside the list comprehension
b' = [1, 2, 3] >>= (\e -> [e' + 1 | let e' = expensive e, even e'])

-- replace (>>=)
c :: [Int]
c = [e' + 1 | e <- [1, 2, 3], let e' = expensive e, even e']

-- do-notation instead of list comprehension
d :: [Int]
d = do
  e <- [1, 2, 3]
  let e' = expensive e
  guard (even e')
  return (e + 1)
