module Lib
  ( main,
  )
where

import Data.Function ((&))
import Data.Maybe (fromMaybe)
import System.Random (randomRIO)
import Text.Read (readMaybe)

main :: IO ()
main = do
  randomNumber <- randomRIO (1, 100)
  print "Guess a number between 1 - 100"
  tryToGuess randomNumber

tryToGuess :: Int -> IO ()
tryToGuess randomNumber = do
  guessedNumber <- readMaybe <$> getLine
  maybe showInputIsNotNumber showGuessResult guessedNumber
  where
    showInputIsNotNumber = print "That was not a number, try again." >> tryToGuess randomNumber
    showGuessResult number = case compare number randomNumber of
      LT -> do
        print "Your namer is too low."
        tryToGuess randomNumber
      GT -> do
        print "Your number is too high."
        tryToGuess randomNumber
      EQ -> print ("Great you guessed correctly, it was '" ++ show randomNumber ++ "'!")
