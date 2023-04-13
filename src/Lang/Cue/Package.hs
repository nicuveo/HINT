module Lang.Cue.Package where

import "this" Prelude

import Document


--------------------------------------------------------------------------------
-- * Package information

data Package = Package
  { pkgName       :: Text
  , pkgDocument   :: Document
  , pkgAttributes :: Attributes
  , pkgFunctions  :: Hashmap Text Function
  }
