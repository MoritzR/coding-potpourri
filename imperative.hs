{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad (when)
import Control.Monad.State (MonadState, State, get)
import Prelude hiding (return, (>>), (>>=))

data New = New

data IfRan = IfRan | IfDidn'tRun

data IndexedState stateBefore stateAfter result = IndexedState {runIndexedState :: stateBefore -> (stateAfter, result)}

ireturn :: a -> IndexedState s s a
ireturn a = IndexedState $ \s -> (s, a)

ibind :: IndexedState p q a -> (a -> IndexedState q r b) -> IndexedState p r b
ibind state f = IndexedState $ \p ->
  let (q, a) = runIndexedState state p
      (r, b) = runIndexedState (f a) q
   in (r, b)

iget :: IndexedState p p p
iget = IndexedState $ \s -> (s, s)

iput :: so -> IndexedState si so ()
iput x = IndexedState (\si -> (x, ()))

(>>=) :: IndexedState p q a -> (a -> IndexedState q r b) -> IndexedState p r b
(>>=) = ibind

return :: a -> IndexedState s s a
return = ireturn

(>>) :: IndexedState p q a -> IndexedState q r b -> IndexedState p r b
s1 >> s2 = s1 >>= \_ -> s2

if' :: Bool -> IndexedState a b () -> IndexedState a IfRan ()
if' True action = action >> iput IfRan
if' False action = iput IfDidn'tRun

else' :: IndexedState IfRan b () -> IndexedState IfRan New ()
else' action = do
  ifRan <- iget
  go ifRan
  where
    go IfRan = iput New
    go IfDidn'tRun = action >> iput New

elif' :: Bool -> IndexedState IfRan b () -> IndexedState IfRan IfRan ()
elif' True action = do
  ifRan <- iget
  go ifRan
  where
    go IfRan = return ()
    go IfDidn'tRun = action >> iput IfRan

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
