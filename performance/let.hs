import Debug.Trace (trace)

-- Compile with 'GHC -O2'

constantLet :: Integer
constantLet =
  let expensive = trace "calculating" (10 * 10)
   in expensive + expensive

constantNoLet :: Integer
constantNoLet = trace "calculating" (10 * 10) + trace "calculating" (10 * 10)

functionLet :: Integer
functionLet =
  let expensive n = trace "calculating" (n * n)
   in expensive 10 + expensive 10

functionNoLet :: Integer
functionNoLet = (\n -> trace "calculating" (n * n)) 10 + (\n -> trace "calculating" (n * n)) 10

main' :: IO ()
main' = do
    print constantLet
    print constantNoLet
    print functionLet
    print functionNoLet

constantLet2 :: Integer -> Integer 
constantLet2 x =
  let expensive = trace "calculating" (x * x)
   in expensive + expensive

constantNoLet2 :: Integer -> Integer 
constantNoLet2 x = trace "calculating" (x * x) + trace "calculating" (x * x)

functionLet2 :: Integer -> Integer 
functionLet2 x =
  let expensive n = trace "calculating" (n * n)
   in expensive x + expensive x

functionNoLet2 :: Integer -> Integer 
functionNoLet2 x = (\n -> trace "calculating" (n * n)) x + (\n -> trace "calculating" (n * n)) x

main :: IO ()
main = do
    print $ constantLet2 10
    print $ constantNoLet2 10
    print $ functionLet2 10
    print $ functionNoLet2 10