{-# LANGUAGE BlockArguments #-}

import Control.Monad (forM_)
import Data.Maybe (listToMaybe)
import System.IO (localeEncoding)
import Text.ParserCombinators.ReadP

main = do
  sqlQuery <- readFile "query.txt"
  forM_ (runParser enumParser sqlQuery)
    putStrLn

runParser parser = head . map fst . readP_to_S parser

anyString = many get

stringInQuotationMarks =
  between
    (char '\'')
    (char '\'')
    (munch (/= '\''))

sqlList =
  between
    (char '(')
    (char ')')
    (stringInQuotationMarks `sepBy` string ", ")

enumParser = do
  anyString
  between
    (string "check (\"status\" in ")
    (char ')')
    sqlList

