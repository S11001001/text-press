module Text.Press.Render
  (
  -- * Rendering whole templates
    doRender
  -- * Template rendering subprocesses
  , lookupVarM
  , showBlock
  , coerceJSToBool
  ) where

import Control.Monad.State
import Control.Monad.Writer.Lazy
import Control.Monad.Error.Class (throwError)

import Data.Map (Map, lookup)
import Data.Maybe (listToMaybe, catMaybes)
import Prelude hiding (lookup)
import Data.List hiding (lookup)

import Text.JSON.Types 
import Text.JSON

import Text.Press.Types

emit s = tell [s]

instance Render Node where 
    render (Text s) = emit s
    render (Var var) = do
        context <- getRenderState
        case lookupVar var context of
            Nothing -> emit ""
            Just jsval -> render jsval
    render (Tag _ f) = render f 

instance Render TagFunc where
    render (TagFunc f) = f 

instance Render JSValue where 
    render JSNull = emit ""
    render (JSString x) = emit $ fromJSString x
    render other = emit $ (showJSValue other) ""

-- | Find a variable.
lookupVarM :: String -> RenderT (Maybe JSValue)
lookupVarM name = do 
    st <- getRenderState 
    return $ lookupVar name st

lookupVar name (RenderState {renderStateValues = vals}) = 
    listToMaybe . catMaybes $ map (getf name) vals

split :: String -> String -> [String]
split tok splitme = unfoldr (sp1 tok) splitme
    where sp1 _ "" = Nothing
          sp1 t s = case find (t `isSuffixOf`) (inits s) of
                      Nothing -> Just (s, "")
                      Just p -> Just (take ((length p) - (length t)) p,
                                      drop (length p) s)

getf :: String -> JSValue -> Maybe JSValue
getf name a = getf' names (Just a)
    where 
        names = split "." name 
        getf' [] y = y
        getf' _ Nothing = Nothing
        getf' (x : xs) (Just (JSObject a)) = getf' xs $ get_field a x
        getf' _ _ = Nothing    

-- | Show a block.
showBlock :: String -> RenderT_ 
showBlock blockName = do
    templates <- templateStack
    let maybeNodes = lookupFirst blockName $ map tmplBlocks $ templates
    case maybeNodes of
        Just nodes -> mapM_ render nodes
        Nothing -> tell [""]

lookupFirst :: Ord k => k -> [Map k a] -> Maybe a
lookupFirst name maps = listToMaybe . catMaybes $ map (lookup name) maps 

getTemplate = fmap renderStateTemplate getRenderState

templateStack = getTemplate >>= templateStack' 
    where
        templateStack' t@(Template {tmplExtends=Nothing}) = return [t]
        templateStack' t@(Template {tmplExtends=Just name}) = do
            cache <- fmap (parserTemplateCache . renderStateParser) get
            case lookup name cache of
                Just template -> do
                    templates <- templateStack' template
                    return $ t : (template : templates)
                Nothing -> throwError $ PressError $ "expecting a template in the cache named: " ++ (show name)

-- | Render the environment's template with the environment's defined
-- variables.
doRender :: RenderT ()
doRender = do 
    bodyNodes <- fmap (tmplNodes . last) templateStack
    mapM_ render bodyNodes

-- | The Jinja-truthiness of a given 'JSValue'.
coerceJSToBool :: JSValue -> Bool
coerceJSToBool JSNull = False 
coerceJSToBool (JSBool bool) = bool
coerceJSToBool (JSRational sign r) = (not sign) && (r > 0)
coerceJSToBool (JSString x) = not (null (fromJSString x))
coerceJSToBool (JSArray vals) = not (null vals)
coerceJSToBool (JSObject obj) = not (null (fromJSObject obj))
