{-# LANGUAGE FlexibleContexts #-}
module Text.Press.Run
  (
  -- * Template rendering
    runJSValuesWithBody
  , runJSValuesWithPath
  , runJSValuesWithTemplate
  -- * Template parsing
  , defaultParser
  , addToTemplateCache
  , lookupTemplates
  ) where

import Control.Monad.State hiding (forM_)
import Control.Monad.Error (runErrorT, ErrorT)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Writer.Lazy hiding (forM_)
import Data.Foldable (forM_)
import Data.Functor ((<$>))
import Prelude hiding (lookup)

import Data.Map (insert, lookup)
import Text.JSON (JSValue, JSValue(..))

import Text.Press.Types
import Text.Press.Parser
import Text.Press.Render
import Text.Press.Tags

type Result = Either PressError [String]

runJSValuesWithPath :: [JSValue] -> String -> IO (Either PressError [String])
runJSValuesWithPath datas templateName = runErrorT $ evalStateT (runJSValuesWithPathStTErrT datas templateName) defaultParser

runJSValuesWithPathStTErrT ::  [JSValue] -> String -> StateT Parser (ErrorT PressError IO) [String]
runJSValuesWithPathStTErrT datas templateName = do
    addToTemplateCache templateName
    parser <- get
    template <- head <$> lookupTemplates templateName
    let st = RenderState {
        renderStateParser = parser,
        renderStateTemplate = template,
        renderStateValues = datas
    }
    lift $ evalStateT (execWriterT doRender) st

runJSValuesWithBody :: [JSValue] -> String -> IO Result
runJSValuesWithBody jsvalues body =
    case parseString defaultParser body of
        Left parsecError -> return $ Left $ ParseError parsecError
        Right template -> runErrorT $ evalStateT (runJSValuesWithTemplateStTErrT jsvalues template) defaultParser

runJSValuesWithTemplate :: [JSValue] -> Template -> Parser -> IO Result
runJSValuesWithTemplate jsvalues template parser = runErrorT $ evalStateT (runJSValuesWithTemplateStTErrT jsvalues template) parser

-- | The lowest-level runJSValues* function.
runJSValuesWithTemplateStTErrT :: [JSValue] -> Template -> StateT Parser (ErrorT PressError IO) [String]
runJSValuesWithTemplateStTErrT jsvalues template = do
    case tmplExtends template of 
        Just s -> addToTemplateCache s
        Nothing -> return ()
    parser <- get
    lift $ evalStateT (execWriterT doRender) RenderState {
        renderStateParser = parser,
        renderStateTemplate = template,
        renderStateValues = jsvalues
    }

lookupTemplates :: (MonadError PressError m, MonadState Parser m)
                => TemplatePath -> m [Template]
lookupTemplates templateName = do
    parser <- get
    case lookup templateName (parserTemplateCache parser) of
        Just template -> case tmplExtends template of 
            Nothing -> return [template]
            Just s -> do 
                xs <- lookupTemplates s
                return $ template : xs
        Nothing -> throwError $ PressError $ "unexpected uncached template: " ++ (show templateName)

addToTemplateCache :: (MonadIO m, MonadState Parser m) => TemplatePath -> m ()
addToTemplateCache template = do
    parser <- get
    let mapping = parserTemplateCache parser
    case lookup template mapping of 
        Just _ -> return ()
        Nothing -> do
            eitherTemplateError <- liftIO $ parseFile parser template
            case eitherTemplateError of
                Left err -> liftIO $ error . show $ err
                Right tmpl -> do 
                    let mapping' = insert template tmpl mapping 
                    put $ parser {parserTemplateCache = mapping'}
                    forM_ (tmplExtends tmpl) addToTemplateCache

-- | A whole-template 'Parser', with 'defaultTagTypes' defined, for
-- use with 'parseFile' or 'parseString'.
defaultParser :: Parser
defaultParser = newParser { parserTagTypes = defaultTagTypes }
