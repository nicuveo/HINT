module Lang.Cue.Representation.Value where

import Control.Monad.Identity
import Data.Kind (Type)

type role Value' nominal
data Value' (f :: Type -> Type)

type Value = Value' Identity
