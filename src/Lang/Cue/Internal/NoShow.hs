module Lang.Cue.Internal.NoShow where

import "this" Prelude

newtype NoShow = NoShow String
  deriving IsString

instance Show NoShow where
  show (NoShow s) = s
