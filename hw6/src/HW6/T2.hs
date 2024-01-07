{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module HW6.T2
  (
    TSet

  , Contains
  , Add
  , Delete
  ) where

import GHC.TypeLits

-- | Type-level set.
type TSet = [Symbol]

-- | Type-level function that checks if a value is in a set.
type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains _ '[] = 'False
  Contains val (val:xs) = 'True
  Contains val (_:xs) = Contains val xs

-- | Type-level function that deletes a value from a set.
type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete _ '[] = '[]
  Delete val (val:xs) = xs
  Delete val (x:xs) = x : Delete val xs

-- | Type-level function that adds a value to a set.
type family Add (v :: Symbol) (set :: TSet) :: TSet where
  Add val lst = val : Delete val lst
