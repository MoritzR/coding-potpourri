{-# LANGUAGE ExistentialQuantification #-}

import Data.Function ((&))
import Data.Maybe (listToMaybe)

data Validation a = forall b.
  Validation
  { preprocess :: a -> Maybe b,
    getError :: b -> String
  }

type Validations a = [Validation a]

testLength :: Validation String
testLength =
  Validation
    { preprocess = Just . length,
      getError = \l -> if l > 3 then "toolong" else ""
    }

testFirstCharacter :: Validation String
testFirstCharacter =
  Validation
    { preprocess = listToMaybe,
      getError = \first -> if first == 'o' then "should not start with o" else ""
    }

validations :: Validations String
validations = [testLength, testFirstCharacter]

-- runValidationBroken :: a -> Validation a -> String
-- runValidationBroken toValidate validation =
--   -- Cannot use record selector ‘getError’ as a function due to escaped type variables
--   -- Probable fix: use pattern-matching syntax instead
--   maybe "" getError validation $ preprocess validation toValidate

runValidation :: a -> Validation a -> String
runValidation toValidate (Validation preprocess getError) =
  maybe "" getError $ preprocess toValidate

runValidations :: a -> Validations a -> [String]
runValidations = map . runValidation
