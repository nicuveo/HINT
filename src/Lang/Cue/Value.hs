module Lang.Cue.Value where

import "this" Prelude

import Data.Sequence    (Seq)

import Lang.Cue.Error
import Lang.Cue.Grammar


--------------------------------------------------------------------------------
-- Value

data CoreValue v
  = Type   Type
  | Atom   Atom
  | Bound  Bound
--  | Struct (Struct v)
  | Bottom BottomSource
  | Top
  | Null
-- | ClosedList [CoreValue v]
-- | OpenList   [CoreValue v] (CoreValue v)
  | Union (Seq (CoreValue v)) -- TODO: come up with a better name?
  | Function Identifier
  | WithDefault v v
  deriving (Show, Functor, Foldable, Traversable)

type Value     = CoreValue BaseValue
type BaseValue = CoreValue Void

data Type
  = BooleanType
  | IntegerType
  | FloatType
  | StringType
  | BytesType
  | StructType Identifier
  deriving (Show, Eq)

data Atom
  = BooleanAtom Bool
  | IntegerAtom Integer
  | FloatAtom   Double
  | StringAtom  Text
  | BytesAtom   Text
  deriving (Show, Eq)

data Bound
  = IntegerBound (OrderedBound Integer)
  | FloatBound   (OrderedBound Double)
  | StringBound  (OrderedBound Text)
  | BytesBound   (OrderedBound Text)
  | RegexBound   [(Text, Bool)]
  deriving (Show, Eq)

data Struct v = StructValue
  { structType   :: Maybe Identifier
  , structFields :: HashMap LabelExpression (CoreValue v)
  }
  deriving (Show, Functor, Foldable, Traversable)


--------------------------------------------------------------------------------
-- Promotion

promote :: BaseValue -> Value
promote = fmap absurd

-- | FIXME: this is probably incorrect wrt. structs
demote :: Value -> BaseValue
demote = fromMaybe (panic DemoteBaseValue) . traverse (const Nothing)


--------------------------------------------------------------------------------
-- Bottom

data BottomSource
  = ArisedFromLiteral
  | UnifyWithNull     Value
  | UnifyTypes        Type       Type
  | UnifyFunctions    Identifier Identifier
  | UnifyAtoms        Atom       Atom
  | UnifyBounds       Bound      Bound
  | UnifyTypeMismatch Value      Value
  | UnifyOOB          Atom       Bound
  | UnsupportedError  Value
  deriving (Show)


--------------------------------------------------------------------------------
-- Bounds checking

data OrderedBound a = OrderedBound
  { lowerBound :: EndPoint a
  , upperBound :: EndPoint a
  , different  :: [a]
  } deriving (Show, Eq, Functor)

data EndPoint a
  = Open
  | Inclusive a
  | Exclusive a
  deriving (Show, Eq, Functor)

checkOrdered :: Ord a => OrderedBound a -> a -> Bool
checkOrdered (OrderedBound l h n) x = checkL && checkH && checkN
  where
    checkL =
      case l of
        Open        -> True
        Inclusive a -> x >= a
        Exclusive a -> x >  a
    checkH =
      case h of
        Open        -> True
        Inclusive a -> x <= a
        Exclusive a -> x <  a
    checkN = x `notElem` n
