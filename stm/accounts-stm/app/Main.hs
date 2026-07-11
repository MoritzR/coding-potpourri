import Control.Concurrent.STM (readTVar, writeTVar, atomically, check, newTVarIO, orElse)
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception (Exception)
import Control.Monad.STM (throwSTM)

withdraw amount account = do
    balance <- readTVar account
    check $ balance >= amount
    writeTVar account (balance - amount)

deposit amount = withdraw (-amount)

transfer from backup to amount = do
  withdraw amount from `orElse` withdraw amount backup
  deposit amount to

main = do
  acc1 <- newTVarIO 200
  acc2 <- newTVarIO 120
  backup <- newTVarIO 10000
  times 300 do
    atomically $ transfer acc1 backup acc2 50 `orElse` throwSTM InsufficentFunds 
    print "done"

data AccountException = InsufficentFunds deriving Show
instance Exception AccountException

















times i = forConcurrently_ [1 .. i] . const
