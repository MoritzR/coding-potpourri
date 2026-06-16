-- import System.IO
import Control.Concurrent.STM (TVar, readTVar, writeTVar, newTVar, STM, retry, atomically, check, newTVarIO)
import Control.Concurrent (threadDelay)

type Account = TVar Int

withdraw :: Account -> Int -> STM ()
withdraw acc amount = do
    bal <- readTVar acc
    check $ bal >= amount
    writeTVar acc (bal - amount)

deposit account amount = withdraw account (-amount)

transfer from to amount = do
  withdraw from amount
  deposit to amount

test = do
  acc1 <- newTVar 200
  acc2 <- newTVar 120
  withdraw acc1 140 -- acc1: 60
  transfer acc1 acc2 50 -- acc1: 10, acc2: 170
  -- transfer acc1 acc2 20 -- acc1: -10

main = do
    atomically test



















check2 theBool =
  if theBool then return ()
  else retry
