{-# LANGUAGE ExistentialQuantification #-}

import Data.Function ((&))
import Data.List (intercalate)
import Data.Maybe (catMaybes, listToMaybe)

data ErrorMesssage = Error String | AllFine deriving (Show)

data Validation a = forall b.
  Validation
  { preprocess :: a -> Maybe b,
    getError :: b -> ErrorMesssage
  }

testLength :: Validation String
testLength =
  Validation
    { preprocess = Just . length,
      getError = \l -> if l > 3 then Error "too long" else AllFine
    }

testFirstCharacter :: Validation String
testFirstCharacter =
  Validation
    { preprocess = listToMaybe,
      getError = \first -> if first == 'o' then Error "should not start with 'o'" else AllFine
    }

-- runValidationBroken :: a -> Validation a -> ErrorMesssage
-- runValidationBroken toValidate validation =
--   -- Cannot use record selector ‘getError’ as a function due to escaped type variables
--   -- Probable fix: use pattern-matching syntax instead
--   maybe AllFine getError validation $ preprocess validation toValidate

runValidation :: a -> Validation a -> Maybe ErrorMesssage
runValidation toValidate (Validation preprocess getError) =
  getError <$> preprocess toValidate

runValidations :: a -> [Validation a] -> [Maybe ErrorMesssage]
runValidations = map . runValidation

asText :: [Maybe ErrorMesssage] -> String
asText validationResults =
  let validationsThatRan = catMaybes validationResults
      errors = [message | (Error message) <- validationsThatRan]
   in concat
        [ "I had ",
          show $ length validationResults,
          " validations and ran ",
          show $ length validationsThatRan,
          " of them. ",
          show $ length errors,
          " returned errors. Here are all errors: ",
          intercalate ", " errors
        ]

main :: IO ()
main = do
  print $ asText $ runValidations "A string to test" [testLength, testFirstCharacter]