{-# LANGUAGE BlockArguments #-}

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
import           Control.Concurrent.Classy.STM.TVar (readTVar, writeTVar)
import           Control.Monad                      (when)
import           Data.Array.Base                    (MArray, getElems,
                                                     getNumElements, newArray,
                                                     readArray, writeArray)
import           Data.Foldable                      (for_)
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
  size <- newTVar 0
  arr <- newArray (0, initCapacity - 1) []
  buckets <- newTVar arr

  return $ CHT
    { chtBuckets = buckets
    , chtSize = size
    }

extractBucket :: (MArray a b m, Hashable k) => k -> a Int b -> m (Int, b)
extractBucket key arr = do
  let keyHash = hash key
  capacity <- getNumElements arr
  let bucketId = keyHash `mod` capacity
  bucket <- readArray arr bucketId
  return (bucketId, bucket)

getCHT
  :: ( MonadConc m
     , Eq k
     , Hashable k
     )
  => k
  -> CHT (STM m) k v
  -> m (Maybe v)
getCHT key cht = atomically do
  arr <- readTVar $ chtBuckets cht
  (_, bucket) <- extractBucket key arr
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
  arr <- readTVar $ chtBuckets cht
  (bucketId, bucket) <- extractBucket key arr
  let (prefix, suffix) = span ((/= key) . fst) bucket
  writeArray arr bucketId $ (key, val) : prefix ++ drop 1 suffix
  when (null suffix) do
    oldSize <- readTVar $ chtSize cht
    writeTVar (chtSize cht) $ oldSize + 1
    oldCapacity <- getNumElements arr

    when (fromIntegral (oldSize + 1) >= fromIntegral oldCapacity * loadFactor) do
      let newCapacity = oldCapacity * 2
      newArr <- newArray (0, newCapacity) []
      arrLst <- getElems arr
      for_ arrLst $ \i ->
        for_ i $ \(key', val') -> do
          (bucketId', bucket') <- extractBucket key' newArr
          writeArray newArr bucketId' $ (key', val') : bucket'
      writeTVar (chtBuckets cht) newArr

sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT cht = atomically $ readTVar $ chtSize cht
