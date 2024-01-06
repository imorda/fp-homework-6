{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module HW6.T2
  ( TSet

  , Contains
  , Add
  , Delete
  ) where

import           GHC.TypeLits

type TSet = [Symbol]

type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains _ '[] = 'False
  Contains val (val:xs) = 'True
  Contains val (_:xs) = Contains val xs

type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete _ '[] = '[]
  Delete val (val:xs) = xs
  Delete val (x:xs) = x : Delete val xs

type family Add (v :: Symbol) (set :: TSet) :: TSet where
  Add val lst = val : Delete val lst
