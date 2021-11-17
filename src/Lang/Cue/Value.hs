module Lang.Cue.Value where

import           Control.Monad
import           Control.Monad.Loops        (unfoldM)
import qualified Control.Monad.State        as MS
import           Data.Char
import           Data.Functor               ((<&>))
import           Data.HashMap.Strict
import qualified Data.List                  as L
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Debug.Trace                (traceM)
import           Text.Megaparsec            hiding (Label, Token, token)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Lang.Cue.Grammar


--------------------------------------------------------------------------------
-- Value

data CoreValue v
  = Type   Type
  | Atom   Atom
  | Bound  Bound
  | Struct (Struct v)
  | Bottom Error
  | Top
  | Null
  | ClosedList [CoreValue v]
  | OpenList   [CoreValue v] (CoreValue v)
  | UnificationOf (CoreValue v) (CoreValue v)
  | DisjunctionOf (CoreValue v) (CoreValue v)
  | WithDefault v v

type Value     = CoreValue BaseValue
type BaseValue = CoreValue Void

type Error = String

data Type
  = BooleanType
  | IntegerType
  | FloatType
  | StringType
  | BytesType
  | StructType Identifier

data Atom
  = BooleanAtom Bool
  | IntegerAtom Integer
  | FloatAtom   Double
  | StringAtom  Text
  | BytesAtom   Text

data Bound
  = IntegerRange (EndPoint Integer, EndPoint Integer)
  | FloatRange   (EndPoint Double,  EndPoint Double)
  | StringRange  (EndPoint Text,    EndPoint Text)
  | BytesRange   (EndPoint Text,    EndPoint Text)
  | MatchesRegex Text
  | Doesn'tMatch Text
  | Different    Atom

data EndPoint a
  = Open
  | Inclusive a
  | Exclusive a

data Struct v = StructValue
  { structType   :: Maybe Identifier
  , structFields :: HashMap LabelExpression (CoreValue v)
  }
