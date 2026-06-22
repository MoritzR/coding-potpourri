import Control.Concurrent.STM (TVar, readTVar, writeTVar, STM, retry, atomically, check, newTVarIO, orElse)
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception (Exception)
import Control.Monad.STM (throwSTM)

type Account = TVar Int

withdraw :: Account -> Int -> STM ()
withdraw acc amount = do
    bal <- readTVar acc
    check $ bal >= amount
    writeTVar acc (bal - amount)

deposit account amount = withdraw account (-amount)

transfer from backup to amount = do
  withdraw from amount `orElse` withdraw backup amount
  deposit to amount

main = do
  acc1 <- newTVarIO 200
  acc2 <- newTVarIO 120
  backup <- newTVarIO 10000
  times 300 do
    atomically $ transfer acc1 backup acc2 50 `orElse` throwSTM InsufficentFunds 
    print "done"

data AccountException = InsufficentFunds deriving Show
instance Exception AccountException
















times i action = forConcurrently_ [(1 :: Int) .. i] (const action)

check2 theBool =
  if theBool
  then return ()
  else retry
