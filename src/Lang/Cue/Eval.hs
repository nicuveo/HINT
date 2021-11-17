module Lang.Cue.Eval where

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
import           Lang.Cue.Value


--------------------------------------------------------------------------------
-- Unification

unify :: CoreValue v -> CoreValue v -> CoreValue v
unify = curry \case
  -- Top is the neutral element
  (Top, v) -> v
  (v, Top) -> v
  -- Bottom always wins
  (Bottom e, _) -> Bottom e
  (_, Bottom e) -> Bottom e
  -- Default case
  (v1, v2) -> UnificationOf v1 v2
