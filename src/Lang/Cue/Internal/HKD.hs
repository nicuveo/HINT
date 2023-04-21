{-# LANGUAGE UndecidableInstances #-}

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

-- | Not all @HKD f@ is a 'Functor', even when 'f' is, because of 'Identity'.
-- This class provides 'hmap', which replaces 'fmap' but deals with the identity
-- case explicitly, and provides internal functions to convert from and to the
-- HKD type family wrapper.
class HKDF f where
  hmap    :: (a -> b) -> HKD f a -> HKD f b
  toHKD   :: f a -> HKD f a
  fromHKD :: HKD f a -> f a

instance {-# OVERLAPPABLE #-} Functor f => HKDF f where
  -- dangerous! doesn't work for Identity
  hmap    = unsafeCoerce (fmap @f)
  toHKD   = unsafeCoerce id
  fromHKD = unsafeCoerce id

instance {-# OVERLAPPING #-} HKDF Identity where
  hmap    = id
  toHKD   = runIdentity
  fromHKD = Identity


--------------------------------------------------------------------------------
-- * FFunctor

type FFunction f g = forall a. f a -> g a

-- | This class allows for generic transformation of HKD types
class FFunctor t where
  ffmap :: forall f g. (HKDF f, HKDF g) => FFunction f g -> t f -> t g

-- | Apply the function by "unwrapping" + "rewrapping" the value.
ffapply :: forall a f g. (HKDF f, HKDF g) => FFunction f g -> HKD f a -> HKD g a
ffapply f x = toHKD @g $ f @a $ fromHKD x

ffrecur :: forall t f g. (HKDF f, HKDF g, FFunctor t) => FFunction f g -> HKD f (t f) -> HKD g (t g)
ffrecur f x = ffapply @(t g) @f @g f $ hmap @f (ffmap @t @f f) x

-- | Hardcoded tranformation to identity.
reify :: forall t f. (HKDF f, FFunctor t) => (forall a. f a -> a) -> t f -> t Identity
reify f = ffmap  (Identity . f)

-- | Hardcoded tranformation from identity.
abstract :: forall t f. (HKDF f, FFunctor t) => (forall a. a -> f a) -> t Identity -> t f
abstract f = ffmap  (f . runIdentity)
