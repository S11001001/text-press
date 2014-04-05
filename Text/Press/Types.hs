module Text.Press.Types
  (
  -- * Templates
    Template(..)
  , TemplatePath
  , Node(..)
  , TagFunc(..)
  , PressError(..)
  , newTemplate
  -- * Parser processes
  , ParserState
  , Parser(..)
  , Expr(..)
  , Token(..)
  , TagType(..)
  , newParser
  -- * Render processes
  , Render
  , render
  , RenderT
  , RenderT_
  , RenderState(..)
  , getRenderState
  , setRenderState
  ) where 

import Control.Monad.Error (ErrorT, Error)
import Control.Monad.Error.Class 
import Control.Monad.State (StateT, get, put)
import Control.Monad.Trans (lift)
import Control.Monad.Writer.Lazy (WriterT)
import Data.Map (Map, lookup, fromList)

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

data TagFunc = TagFunc RenderT_

data Node = Var String
    | Tag TagName TagFunc
    | Text String
    deriving (Show)

instance Show TagFunc where
    show s = "TagFunc ?"

type TemplatePath = String

data Template = Template {
    tmplExtends :: Maybe TemplatePath,
    tmplBlocks :: Map String [Node],
    tmplNodes :: [Node],
    tmplFilename :: String
} deriving (Show) 

newTemplate = Template Nothing (fromList []) [] "" 

type TagName = String
type TemplateParser a = Prim.Parsec [(Token, SourcePos)] ParserState a
type NodeParser = TemplateParser (Maybe Node)
data TagType = TagType (TagName -> String -> NodeParser)
data Expr = ExprStr String
    | ExprVar String
    | ExprNum Double
    deriving (Ord, Eq, Show)

type ParserState = (Parser, Template)

instance Show TagType where
    show s = "TagType ?"

data Token = PText String 
    | PTag TagName String
    | PVar String
    deriving (Ord, Show, Eq)

data Parser = Parser {
    parserTagTypes :: Map TagName TagType,
    parserSearchPaths :: [String],
    parserTemplateCache :: Map TemplatePath Template 
} deriving (Show)

class Render a where
    render :: a -> RenderT_

newParser = Parser (fromList []) [] (fromList [])

