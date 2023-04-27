module Lang.Cue.Value where

import Data.Functor.Const
import Control.Monad.Identity
import Data.Kind (Type)

import {-# SOURCE #-} Lang.Cue.IR (Thunk)

type role Value' nominal
data Value' (f :: Type -> Type)

type Value     = Value' Identity
type WHNFValue = Value' (Const Thunk)
