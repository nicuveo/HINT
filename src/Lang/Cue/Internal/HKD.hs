module Lang.Cue.Internal.HKD where

import "this" Prelude

import Unsafe.Coerce


--------------------------------------------------------------------------------
-- * HKD

-- | This type family allows for erasure of 'Identity' in HKD types.
--
-- Given a type @A'@ such that
--
--     data A' f = A
--       { x :: HKD f Int
--       , y :: HKD f Int
--       }
--
-- It means that @A' Identity@ resolves to:
--
--     data A = A
--       { x :: Int
--       , y :: Int
--       }
--
-- This makes working with HKD types easier, since it makes it possible to deal
-- with individual fields without having to manually coerce to and from
-- @Identity@.
type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a


--------------------------------------------------------------------------------
-- * HKD conversion
--
-- We can always convert between @f a@ and @HKD f a@, since for all cases but
-- @Identity@, @f a ~ HKD f a@, and for @Identity@ the conversion is trivial.
-- That means that in all cases, they are representationally equivalent, and the
-- conversion could be performed by 'coerce'. However, using @coerce@ would mean
-- delegating @forall a. Coercible (f a) (HKD f a)@ and @forall a. Coercible
-- (HKD f a) (f a)@ instances all the way to the call sites where @f@ is known,
-- and quantified constraints don't play well with non-injective type
-- families...
--
-- As a result, since we KNOW that @HKD f a@ and @f a@ are representationally
-- equivalent for all @f@ and @a@ WITHOUT EXCEPTION, we can safely use the
-- dreaded 'unsafeCoerce'.

toHKD :: forall f a. f a -> HKD f a
toHKD = unsafeCoerce

fromHKD :: forall f a. HKD f a -> f a
fromHKD = unsafeCoerce


--------------------------------------------------------------------------------
-- * Functor-like functions

-- | @HKD f@ is not a functor for all @f@ because of @Identity@; furthermore
-- @HKD f@ is nonsensical by itself since it's a partial type family
-- application. We therefore can't define @fmap@ for @HKD f@, but we can define
-- an equivalent @hmap@ with identical semantics.
hmap :: forall f a b. Functor f => (a -> b) -> HKD f a -> HKD f b
hmap f = toHKD @f . fmap f . fromHKD @f

-- | Similarly to 'hmap', this provides an equivalent to 'pure'.
hpure :: forall f a. Applicative f => a -> HKD f a
hpure = toHKD @f . pure

type FFunction f g = forall a. f a -> g a

-- | This class describes how a HKD type is kind of a "higher-kind functor": we
-- can convert from @A f@ to @A g@ given a function that converts from @f a@ to
-- @g a@ for all @a@.
class FFunctor t where
  ffmap :: forall f g. (Functor f, Functor g) => FFunction f g -> t f -> t g

-- | Apply a @f a -> g a@ function on a @HKD f a@.
--
-- Used to implement @FFunctor@ instances without having to manually convert
-- back and forth with 'toHKD' and 'fromHKD'.
ffapply :: forall a f g. FFunction f g -> HKD f a -> HKD g a
ffapply f = toHKD @g . f @a . fromHKD @f

-- | Apply a @f a -> g a@ function on a @HKD f a@ where @a@ is itself a HKD type
-- parameterized by @f@: this performs a recurvise @ffmap@, and uses @ffaply@ on
-- the result.
ffrecur :: forall t f g. (Functor f, Functor g, FFunctor t) => FFunction f g -> HKD f (t f) -> HKD g (t g)
ffrecur f = ffapply @(t g) f . hmap @f (ffmap @t f)

-- | Special case of @ffmap@ where the target functor is @Identity@.
--
-- This allows the caller to provide a @f a -> a@ function instead of having to
-- use an explicit @f a -> Identity a@ one.
reify :: (Functor f, FFunctor t) => (forall a. f a -> a) -> t f -> t Identity
reify f = ffmap  (Identity . f)

-- | Special case of @ffmap@ where the source functor is @Identity@.
--
-- This allows the caller to provide a @a -> f a@ function instead of having to
-- use an explicit @Identity a -> f a@ one.
abstract :: (Functor f, FFunctor t) => (forall a. a -> f a) -> t Identity -> t f
abstract f = ffmap  (f . runIdentity)
