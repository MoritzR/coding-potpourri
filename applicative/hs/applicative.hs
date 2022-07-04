import Control.Applicative (liftA2)

addNumbers :: Int -> Int -> Int
addNumbers = (+)

addLists :: [Int] -> [Int] -> [Int]
addLists = liftA2 addNumbers

resultList :: [Int]
resultList = addLists [1, 2] [3, 4]

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe = liftA2 addNumbers

resultMaybe :: Maybe Int
resultMaybe = addMaybe (Just 1) (Just 2)

addFunction :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
addFunction = liftA2 addNumbers

resultFunction :: Int -> Int
resultFunction = addFunction (*2) (+3)

-- now with applicative

resultListApplicative :: [Int]
resultListApplicative = addNumbers <$> [1, 2] <*> [3, 4]

resultMaybeApplicative :: Maybe Int
resultMaybeApplicative = addNumbers <$> Just 1 <*> Just 2

resultFunctionApplicative :: Int -> Int
resultFunctionApplicative = addNumbers <$> (*2) <*> (+3)

main :: IO ()
main = do
    print resultList
    print resultListApplicative

    print resultMaybe
    print resultMaybeApplicative

    print $ resultFunction 10
    print $ resultFunctionApplicative 10