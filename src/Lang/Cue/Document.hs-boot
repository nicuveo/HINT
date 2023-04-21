module Lang.Cue.Document where

import Control.Monad.Identity
import Data.Kind (Type)

type role Document' nominal
data Document' (f :: Type -> Type)
type Document = Document' Identity
