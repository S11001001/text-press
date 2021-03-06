{-# LANGUAGE ScopedTypeVariables #-}

module Text.Press.Tags
  (
  -- * Defined tag types
    defaultTagTypes
  ) where
import Text.JSON.Types

import Data.Functor ((<$>))
import Data.Map (Map, fromList, insert)
import Data.Maybe (catMaybes)
import qualified Text.Parsec.Prim as Parsec.Prim
import Text.Parser.Combinators (manyTill, unexpected)
import Control.Monad (forM_)

import Text.Press.Parser
import Text.Press.Render
import Text.Press.Types

extendsTag name rest = do
    exprs <- runParseTagExpressions rest
    include <- case exprs of
                (ExprStr s : _) -> return s
                _ -> fail "expecting a string"
    let rest' = Just . strip $ include
    Parsec.Prim.modifyState $ \(parser, tmpl) -> (parser, tmpl {tmplExtends = rest'})
    return $ Nothing

blockTag name rest = do
    exprs <- runParseTagExpressions rest
    blockName <- case exprs of
        (ExprVar var : _) -> return var
        _ -> unexpected (show otherwise)
    nodes <- catMaybes <$> manyTill pNode (tagNamed "endblock")
    Parsec.Prim.modifyState $ \(parser, tmpl) -> (parser,
        tmpl {tmplBlocks = insert blockName nodes (tmplBlocks tmpl)})
    return $ Just $ Tag "block" $ TagFunc $ showBlock blockName

-- | This is mapping of all of the default tag types. 
defaultTagTypes :: Map String TagType
defaultTagTypes = (fromList [
    ("extends", TagType extendsTag),
    ("block", TagType blockTag),
    ("if", TagType ifTag),
    ("for", TagType forTag),
    ("comment", TagType commentTag)
    ])

-- Comment Tag
commentTag _ _ = do 
    manyTill pNode (tagNamed "endcomment")
    return Nothing

-- If tag
ifTag name rest = do
        expr <- parseIfExpr rest
        scan [] expr
    where
        scan ifs e = do
            (maybeNodes, tokenPos) <- manyTill' pNode (tagNamedOneOf ["else", "endif", "elif"])
            let nodes = catMaybes maybeNodes
            let token = fst tokenPos
            let ifs' = ifs ++ [(e, nodes)]
            case token of
                (PTag "endif" rest) ->
                    return $ Just $ Tag "if" $ TagFunc $ showIfElse ifs' []
                (PTag "elif" rest) -> do
                    e' <- parseIfExpr rest
                    scan ifs' e'
                (PTag "else" rest) -> do
                    nodes <- catMaybes <$> manyTill pNode (tagNamed "endif")
                    return $ Just $ Tag "if" $ TagFunc $ showIfElse ifs' nodes
                _ -> unexpected "unexpected tag"
        parseIfExpr s = do
            exprs <- runParseTagExpressions s
            case exprs of
                [] -> unexpected "empty if"
                (x : []) -> return x
                (_ : xs) -> unexpected $ show . head $ xs

-- Version of manyTill that returns the terminating token
manyTill' p1 p2 = scan
    where scan = (Parsec.Prim.try p2') Parsec.Prim.<|> p1'
          p1' = do x <- p1
                   (xs, y) <- scan
                   return (x : xs, y)
          p2' = do y <- p2
                   return ([], y)

-- Evaluate an expression to a boolean suitable for an If clase
exprToBool :: Expr -> RenderT Bool
exprToBool expr =
    case expr of
       ExprStr s -> return $ not (null s)
       ExprNum num -> return $ num > 0
       ExprVar var -> do
            maybeVal <- lookupVarM var
            case maybeVal of
                Nothing -> return False
                Just val -> return $ coerceJSToBool val

showIfElse :: [(Expr, [Node])] -> [Node] -> RenderT_
showIfElse [] right = mapM_ render right
showIfElse ((expr, left) : xs) right = do
    succ <- exprToBool expr
    if succ
        then mapM_ render left
        else showIfElse xs right

forTag name rest = do
    (target, sourceExpr) <- parseFor rest
    (maybeNodes, (token, _)) <- manyTill' pNode (tagNamedOneOf ["endfor", "else"])
    let forNodes = catMaybes maybeNodes
    case token of
        PTag "else" _ -> do
            elseNodes <- catMaybes <$> manyTill pNode (tagNamed "endfor")
            return $ Just $ Tag "for" $ TagFunc $ showFor target sourceExpr forNodes elseNodes
        PTag "endfor" _ ->
            return $ Just $ Tag "for" $ TagFunc $ showFor target sourceExpr forNodes []

    where
        parseFor s = do
             exprs <- runParseTagExpressions s
             if length exprs == 3
                then do
                    target <- case head exprs of
                        ExprVar x -> return x
                        _ -> fail "unexpected for target"
                    case head $ tail exprs of
                        ExprVar "in" -> return ()
                        _ -> fail "expecting 'in'"
                    return $ (target, head $ tail $ tail exprs)
                else unexpected "number of arguments"

showFor :: String -> Expr -> [Node] -> [Node] -> RenderT_
showFor target sourceExpr forNodes elseNodes = do
        sourceValues <- toList <$> toJS sourceExpr
        runFor sourceValues
    where
        runFor [] = mapM_ render elseNodes
        runFor vals = forM_ vals $ \x -> do
            pushValues [(target, x)]
            mapM_ render forNodes
            popValues

        toList (Just (JSArray vals)) = vals
        toList _ = []

        toJS (ExprVar x) = lookupVarM x
        toJS _ = return Nothing

        pushValues kvpairs = do
            st <- getRenderState
            setRenderState $ st {
                renderStateValues = values' : renderStateValues st
            }
            where values' = JSObject $ toJSObject kvpairs

        popValues = do
            st <- getRenderState
            setRenderState $ st {
                renderStateValues = tail $ renderStateValues st
            }

