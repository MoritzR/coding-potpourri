{-# LANGUAGE ExistentialQuantification #-}

import Data.Function ((&))
import Data.Maybe (listToMaybe)

data ErrorMesssage = Error String | AllFine

data Validation a = forall b.
  Validation
  { preprocess :: a -> Maybe b,
    getError :: b -> ErrorMesssage
  }

type Validations a = [Validation a]

testLength :: Validation String
testLength =
  Validation
    { preprocess = Just . length,
      getError = \l -> if l > 3 then Error "toolong" else AllFine
    }

testFirstCharacter :: Validation String
testFirstCharacter =
  Validation
    { preprocess = listToMaybe,
      getError = \first -> if first == 'o' then Error "should not start with o" else AllFine
    }

validations :: Validations String
validations = [testLength, testFirstCharacter]

-- runValidationBroken :: a -> Validation a -> ErrorMesssage
-- runValidationBroken toValidate validation =
--   -- Cannot use record selector ‘getError’ as a function due to escaped type variables
--   -- Probable fix: use pattern-matching syntax instead
--   maybe AllFine getError validation $ preprocess validation toValidate

runValidation :: a -> Validation a -> ErrorMesssage
runValidation toValidate (Validation preprocess getError) =
  maybe AllFine getError $ preprocess toValidate

runValidations :: a -> Validations a -> [ErrorMesssage]
runValidations = map . runValidation
