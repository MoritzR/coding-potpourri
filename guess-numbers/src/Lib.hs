module Lib
  ( main,
  )
where

import Data.Function ((&))
import System.Random (randomRIO)

main :: IO ()
main = do
  randomNumber <- randomRIO (1, 100)
  print "Guess a number between 1 - 100"
  tryToGuess randomNumber

tryToGuess :: Int -> IO ()
tryToGuess randomNumber = do
  guessedNumber <- readLn
  case compare guessedNumber randomNumber of
    LT -> do
      print "Your namer is too low."
      tryToGuess randomNumber
    GT -> do
      print "Your number is too high."
      tryToGuess randomNumber
    EQ -> print ("Great you guessed correctly, it was '" ++ show randomNumber ++ "'!")

