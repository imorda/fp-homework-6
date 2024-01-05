{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module HW6.T1
  ( BucketsArray
  , CHT (..)

  , newCHT
  , getCHT
  , putCHT
  , sizeCHT

  , initCapacity
  , loadFactor
  ) where

import           Control.Concurrent.Classy          (MonadConc, STM, atomically)
import           Control.Concurrent.Classy.STM      (TArray, TVar, newTVar)
import           Control.Concurrent.Classy.STM.TVar (readTVar)
import           Control.Monad.STM.Class            (MonadSTM)
import           Data.Array.Base                    (MArray, getNumElements,
                                                     newArray, readArray,
                                                     writeArray)
import           Data.Hashable                      (Hashable, hash)

initCapacity :: Int
initCapacity = 16

loadFactor :: Double
loadFactor = 0.75

type Bucket k v = [(k, v)]
type BucketsArray stm k v = TArray stm Int (Bucket k v)

data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  }

newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT = atomically do
  size <- newTVar initCapacity
  arr <- newArray (0, initCapacity - 1) []
  buckets <- newTVar arr

  return $ CHT
    { chtBuckets = buckets
    , chtSize = size
    }

extractBucket
  :: ( Hashable a
     , Control.Monad.STM.Class.MonadSTM m
     , MArray (TArray stm) (Bucket k v) m
     , TVar stm ~ TVar m
     )
  => a
  -> CHT stm k v
  -> m (TArray stm Int (Bucket k v), Int, Bucket k v)
extractBucket key cht = do
  let keyHash = hash key
  arr <- readTVar $ chtBuckets cht
  capacity <- getNumElements arr
  let bucketId = keyHash `mod` capacity
  bucket <- readArray arr bucketId
  return (arr, bucketId, bucket)

getCHT
  :: ( MonadConc m
     , Eq k
     , Hashable k
     )
  => k
  -> CHT (STM m) k v
  -> m (Maybe v)
getCHT key cht = atomically do
  (_, _, bucket) <- extractBucket key cht
  return $ lookup key bucket

putCHT
  :: ( MonadConc m
     , Eq k
     , Hashable k
     )
  => k
  -> v
  -> CHT (STM m) k v
  -> m ()
putCHT key val cht = atomically do
  (arr, bucketId, bucket) <- extractBucket key cht
  let (prefix, suffix) = span ((/= key) . fst) bucket
  writeArray arr bucketId $ (key, val) : prefix ++ drop 1 suffix

sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT cht = atomically $ readTVar $ chtSize cht
