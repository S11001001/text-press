{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright :  (c) 2009 Brandon Bickford, (c) 2014 Stephen Compall
-- License   :  GPL
-- Maintainer:  Brandon Bickford <bickfordb@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- These are the types used in template models, parsing, and
-- rendering.

module Text.Press.Types
  (
  -- * Templates
    Template(..)
  , TemplatePath
  , Node(..)
  , TagFunc(..)
  , PressError(..)
  , newTemplate
  -- * Custom tags
  , TagName
  , TagType(..)
  -- * Parser processes
  , ParserState
  , Parser(..)
  , Expr(..)
  , Token(..)
  , newParser
  -- * Render processes
  , Render(..)
  , RenderT
  , RenderT_
  , RenderState(..)
  , getRenderState
  , setRenderState
  ) where 

import GHC.Exts (Constraint)

import Control.Monad.Error (ErrorT)
import Control.Monad.Error.Class 
import Control.Monad.State (StateT, get, put)
import Control.Monad.Writer.Lazy (WriterT)
import Data.Map (Map, fromList)

import qualified Text.Parsec.Prim as Prim
import qualified Text.Parsec.Error
import Text.Parsec.Pos (SourcePos)
import Text.JSON (JSValue)

-- | The state member of 'RenderT'.
data RenderState m = RenderState {
    renderStateParser :: Parser m,
    renderStateTemplate :: Template m,
    renderStateValues :: [JSValue]
}

-- | An error that occurred during template parsing or rendering.
data PressError = PressError String
    | ParseError Text.Parsec.Error.ParseError
    | RenderError String
    deriving (Show)

instance Error PressError where
    noMsg = PressError "Some rendering error"
    strMsg s = PressError s

-- | The transformer stack for template-rendering actions.
type RenderT m = WriterT [String] (StateT (RenderState m) (ErrorT PressError m))

-- | A 'RenderT' with no value.
type RenderT_ m = RenderT m ()

-- | Type-constrained 'get'.
getRenderState :: Monad m => RenderT m (RenderState m)
--getRenderState = lift $ lift $ get
getRenderState = get

-- | Type-constrained 'put'.
setRenderState :: Monad m => RenderState m -> RenderT m ()
setRenderState = put

-- | Rendering functions get stored in the template state; this marks
-- those functions so we can 'show' them and 'render' them.
newtype TagFunc m = TagFunc (RenderT_ m)

-- | An element of the parsed template tree.
data Node m = Var String
    | Tag TagName (TagFunc m)
    | Text String
    deriving (Show)

instance Show (TagFunc m) where
    show _ = "TagFunc ?"

-- | The lookup key of 'parserTemplateCache'.
type TemplatePath = String

-- | A parsed Jinja template.
data Template m = Template {
    tmplExtends :: Maybe TemplatePath,
    tmplBlocks :: Map String [Node m],
    tmplNodes :: [Node m],
    tmplFilename :: String
} deriving (Show) 

-- | An empty 'Template'.
newTemplate :: Template m
newTemplate = Template Nothing (fromList []) [] "" 

type TagName = String
type TemplateParser m a = Prim.Parsec [(Token, SourcePos)] (ParserState m) a
type NodeParser m = TemplateParser m (Maybe (Node m))

-- | Configurable member of 'parserTagTypes' for parsing arguments to
-- one particular tag.
newtype TagType m = TagType (TagName -> String -> NodeParser m)

-- | An argument to a 'Tag'.
data Expr = ExprStr String
    | ExprVar String
    | ExprNum Double
    deriving (Ord, Eq, Show)

-- | State of 'TemplateParser'.
type ParserState m = (Parser m, Template m)

instance Show (TagType m) where
    show _ = "TagType ?"

-- | An 'Expr' with unresolved 'Tag's.
data Token = PText String 
    | PTag TagName String
    | PVar String
    deriving (Ord, Show, Eq)

-- | Configuration for parsing a particular template.
data Parser m = Parser {
    -- | All defined tags, e.g. "extends", "for".  A sensible default
    -- is 'Text.Press.Tags.defaultTagTypes'; merge that with your own
    -- additions to extend the default set.
    parserTagTypes :: Map TagName (TagType m),
    -- | Unused.
    parserSearchPaths :: [String],
    -- | A cache where 'Text.Press.Run.runJSValuesWithBody' et al look
    -- for already-parsed templates.
    parserTemplateCache :: Map TemplatePath (Template m)
} deriving (Show)

-- | Types that can be rendered as part of a template rendering
-- process.
class Render a where
    -- | The operations that the transformed rendering stack needs.
    type RenderIn a m :: Constraint

    -- | Write strings representing the argument to the writer part of
    -- the 'RenderT_' stack.
    render :: RenderIn a m => a -> RenderT_ m

-- | An empty 'Parser'.
newParser :: Parser m
newParser = Parser (fromList []) [] (fromList [])

