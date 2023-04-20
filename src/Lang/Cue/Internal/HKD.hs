{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Lang.Cue.Internal.HKD where

import "this" Prelude

import Unsafe.Coerce


--------------------------------------------------------------------------------
-- * HKD

-- | This class allows for erasure of identity in HKD types such as token.
type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a


--------------------------------------------------------------------------------
-- * HKDF

-- | Not all @HKD f@ is a 'Functor', even when 'f' is, because of
-- 'Identity'. This class provides 'hmap', which replaces 'fmap' but deals with
-- the identity case explicitly.
class HKDF f where
  hmap :: (a -> b) -> HKD f a -> HKD f b

instance {-# OVERLAPPABLE #-} Functor f => HKDF f where
  -- dangerous! doesn't work for Identity
  hmap = unsafeCoerce (fmap @f)

instance {-# OVERLAPPING #-} HKDF Identity where
  hmap = id


--------------------------------------------------------------------------------
-- * FFunctor

-- | This class allows for generic transformation of HKD types
class FFunctor t where
  ffmap :: (forall a. HKD f a -> HKD g a) -> t f -> t g

-- | Hardcoded tranformation to identity.
reify :: FFunctor t => (forall a. HKD f a -> a) -> t f -> t Identity
reify = ffmap
