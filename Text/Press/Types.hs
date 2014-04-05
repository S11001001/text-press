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
data RenderState = RenderState {
    renderStateParser :: Parser,
    renderStateTemplate :: Template,
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

-- | The transformer stack for template-rendering actions, applied to
-- 'IO'.
type RenderT = WriterT [String] (StateT RenderState (ErrorT PressError IO))

-- | A 'RenderT' with no value.
type RenderT_ = RenderT ()

-- | Type-constrained 'get'.
getRenderState :: RenderT RenderState
--getRenderState = lift $ lift $ get
getRenderState = get

-- | Type-constrained 'put'.
setRenderState :: RenderState -> RenderT ()
setRenderState = put

-- | Rendering functions get stored in the template state; this marks
-- those functions so we can 'show' them and 'render' them.
newtype TagFunc = TagFunc RenderT_

-- | An element of the parsed template tree.
data Node = Var String
    | Tag TagName TagFunc
    | Text String
    deriving (Show)

instance Show TagFunc where
    show _ = "TagFunc ?"

-- | The lookup key of 'parserTemplateCache'.
type TemplatePath = String

-- | A parsed Jinja template.
data Template = Template {
    tmplExtends :: Maybe TemplatePath,
    tmplBlocks :: Map String [Node],
    tmplNodes :: [Node],
    tmplFilename :: String
} deriving (Show) 

-- | An empty 'Template'.
newTemplate :: Template
newTemplate = Template Nothing (fromList []) [] "" 

type TagName = String
type TemplateParser a = Prim.Parsec [(Token, SourcePos)] ParserState a
type NodeParser = TemplateParser (Maybe Node)

-- | Configurable member of 'parserTagTypes' for parsing arguments to
-- one particular tag.
newtype TagType = TagType (TagName -> String -> NodeParser)

-- | An argument to a 'Tag'.
data Expr = ExprStr String
    | ExprVar String
    | ExprNum Double
    deriving (Ord, Eq, Show)

-- | State of 'TemplateParser'.
type ParserState = (Parser, Template)

instance Show TagType where
    show _ = "TagType ?"

-- | An 'Expr' with unresolved 'Tag's.
data Token = PText String 
    | PTag TagName String
    | PVar String
    deriving (Ord, Show, Eq)

-- | Configuration for parsing a particular template.
data Parser = Parser {
    -- | All defined tags, e.g. "extends", "for".  A sensible default
    -- is 'Text.Press.Tags.defaultTagTypes'; merge that with your own
    -- additions to extend the default set.
    parserTagTypes :: Map TagName TagType,
    -- | Unused.
    parserSearchPaths :: [String],
    -- | A cache where 'Text.Press.Run.runJSValuesWithBody' et al look
    -- for already-parsed templates.
    parserTemplateCache :: Map TemplatePath Template 
} deriving (Show)

-- | Types that can be rendered as part of a template rendering
-- process.
class Render a where
    -- | Write strings representing the argument to the writer part of
    -- the 'RenderT_' stack.
    render :: a -> RenderT_

-- | An empty 'Parser'.
newParser :: Parser
newParser = Parser (fromList []) [] (fromList [])

