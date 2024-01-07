{-# LANGUAGE BlockArguments #-}

module HW6.T1
  (
  -- * Types
    BucketsArray
  , CHT (..)

  -- * Functions
  , newCHT
  , getCHT
  , putCHT
  , sizeCHT

  -- * Constants
  , initCapacity
  , loadFactor
  ) where

import Control.Concurrent.Classy (MonadConc, STM, atomically)
import Control.Concurrent.Classy.STM (TArray, TVar, newTVar)
import Control.Concurrent.Classy.STM.TVar (readTVar, writeTVar)
import Control.Monad (when)
import Data.Array.Base (MArray, getElems, getNumElements, newArray, readArray, writeArray)
import Data.Foldable (for_)
import Data.Hashable (Hashable, hash)

-- | Initial number of buckets in the CHT.
initCapacity :: Int
initCapacity = 16

-- | Maximum load factor of the CHT before resizing.
loadFactor :: Double
loadFactor = 0.75

-- | Type of a bucket in the CHT.
type Bucket k v = [(k, v)]
-- | Type of the array of buckets in the CHT.
type BucketsArray stm k v = TArray stm Int (Bucket k v)

-- | Type of the CHT.
data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v) -- ^ Array of buckets.
  , chtSize    :: TVar stm Int -- ^ Number of elements in the CHT.
  }

-- | Create a new empty CHT.
newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT = atomically do
  size <- newTVar 0
  arr <- newArray (0, initCapacity - 1) []
  buckets <- newTVar arr

  return $ CHT
    { chtBuckets = buckets
    , chtSize = size
    }

-- | Extract a bucket from the array of buckets.
extractBucket :: (MArray a b m, Hashable k) => k -> a Int b -> m (Int, b)
extractBucket key arr = do
  let keyHash = hash key
  capacity <- getNumElements arr
  let bucketId = keyHash `mod` capacity
  bucket <- readArray arr bucketId
  return (bucketId, bucket)

-- | Get a value from the CHT by key.
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

-- | Put a value into the CHT by key.
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
      newArr <- newArray (0, newCapacity - 1) []
      arrLst <- getElems arr
      for_ arrLst $ \i ->
        for_ i $ \(key', val') -> do
          (bucketId', bucket') <- extractBucket key' newArr
          writeArray newArr bucketId' $ (key', val') : bucket'
      writeTVar (chtBuckets cht) newArr

-- | Get the number of elements in the CHT.
sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT cht = atomically $ readTVar $ chtSize cht
