{-# LANGUAGE TypeFamilyDependencies #-}

module Lang.Cue.HKD where

import "this" Prelude


type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

class FFunctor t where
  ffmap :: (forall a. HKD f a -> HKD g a) -> t f -> t g


reify :: FFunctor t => (forall a. HKD f a -> a) -> t f -> t Identity
reify = ffmap
