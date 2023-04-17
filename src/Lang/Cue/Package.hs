module Lang.Cue.Package where

import "this" Prelude

import Lang.Cue.Document


--------------------------------------------------------------------------------
-- * Package information

data Package = Package
  { pkgName       :: Text
  , pkgDocument   :: Thunk
  , pkgAttributes :: Attributes
  , pkgFunctions  :: HashMap Text Function
  }
