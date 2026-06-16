-- import System.IO
import Control.Concurrent.STM (TVar, readTVar, writeTVar, STM, retry, atomically, check, newTVarIO)
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception (Exception)
import Control.Monad.STM (throwSTM)
import Control.Monad (unless)

type Account = TVar Int

withdraw :: Account -> Int -> STM ()
withdraw acc amount = do
    bal <- readTVar acc
    checkAtLeast bal amount
    check $ bal >= amount
    writeTVar acc (bal - amount)

deposit account amount = withdraw account (-amount)

transfer from to amount = do
  withdraw from amount
  deposit to amount

test acc1 acc2 = do
  withdraw acc1 140 -- acc1: 60
  transfer acc1 acc2 50 -- acc1: 10, acc2: 170
  -- transfer acc1 acc2 20 -- acc1: -10

main = do
  acc1 <- newTVarIO 200
  acc2 <- newTVarIO 120
  times 2 do
    atomically $ test acc1 acc2
    print "done"


data AccountException = InsufficentFunds Int Int
instance Exception AccountException
instance Show AccountException where
  show (InsufficentFunds has needs) = "Account has " ++ show has ++ " funds, but needs at least " ++ show needs ++ "."

checkAtLeast bal amount = unless (bal >= amount) $ throwSTM $ InsufficentFunds bal amount















times i action = forConcurrently_ [(1 :: Int) .. i] (const action)

check2 theBool =
  if theBool then return ()
  else retry
