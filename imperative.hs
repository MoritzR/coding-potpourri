{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RebindableSyntax #-}

import Control.Monad (when)
import Control.Monad.State (MonadState, State)
import Prelude hiding (return, (>>), (>>=))

data New = New

data IfRan = IfRan | IfDidn'tRun

data IndexedState stateBefore stateAfter result = IndexedState {runIndexedState :: stateBefore -> (stateAfter, result)}

get :: IndexedState p p p
get = IndexedState $ \s -> (s, s)

put :: so -> IndexedState si so ()
put x = IndexedState (\si -> (x, ()))

(>>=) :: IndexedState p q a -> (a -> IndexedState q r b) -> IndexedState p r b
s >>= f = IndexedState $ \p ->
  let (q, a) = runIndexedState s p
      (r, b) = runIndexedState (f a) q
   in (r, b)

(>>) :: IndexedState p q a -> IndexedState q r b -> IndexedState p r b
s1 >> s2 = s1 >>= \_ -> s2

return :: a -> IndexedState s s a
return a = IndexedState $ \s -> (s, a)

if' :: Bool -> IndexedState a b () -> IndexedState a IfRan ()
if' True action = action >> put IfRan
if' False action = put IfDidn'tRun

else' :: IndexedState IfRan b () -> IndexedState IfRan New ()
else' action = do
  ifRan <- get
  go ifRan
  where
    go IfRan = put New
    go IfDidn'tRun = action >> put New

elif' :: Bool -> IndexedState IfRan b () -> IndexedState IfRan IfRan ()
elif' True action = do
  ifRan <- get
  go ifRan
  where
    go IfRan = return ()
    go IfDidn'tRun = action >> put IfRan

def :: IndexedState New b a -> a
def state = snd $ runIndexedState state New

prog1 :: ()
prog1 = def do
  if' True do
    return ()

  elif' True do
    return ()

  elif' True do
    return ()

  else' do
    return ()

-- fails to compile
-- prog2 :: ()
-- prog2 = def do
--   else' do
--     return ()

-- fails to compile
-- prog3 :: ()
-- prog3 = def do
--   if' True do
--     return ()
--   else' do
--     return ()
--   else' do
--     return ()
