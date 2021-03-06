
module Text.Press.Parser
  ( 
  -- * Template element parsers
    runParseTagExpressions
  , pNode
  , tagNamed
  , tagNamedOneOf
  -- * Parser utilities
  , parseFile
  , parseString
  -- * String utilities
  , strip
  ) where

import Control.Applicative ((<|>), (*>))
import Data.Char (isSpace)
import Data.Functor ((<$>))
import Data.Map (lookup)
import Data.Maybe (catMaybes)
import Prelude hiding (lookup)

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec.Error
import Text.Parsec.String (parseFromFile)
import Text.Parsec.Pos (SourcePos, sourceName)
import Text.Parsec.Prim (Parsec, getPosition, getState)
import qualified Text.Parsec.Prim as Parsec.Prim
import Text.Parser.Char (alphaNum, anyChar, letter, oneOf, space, string)
import Text.Parser.Combinators (choice, eof, many, manyTill, optional, sepEndBy, skipMany, some, try)
import Text.Parser.LookAhead (lookAhead)

import Text.Press.Types

-- | The parser of 'Expr's with unresolved 'Tag's.
intermediateParser = manyTill intermediate eof 
    
intermediate = choice [try parseTag, try parseVar, someText]

someText = withPos $ fmap PText someText' 
    where someText' = (choice [check $ string "{{", check $ string "{%", check eof]) <|> succ
          check p = (lookAhead $ try p) *> return []
          succ = do 
            c <- anyChar
            xs <- someText'
            return $ c : xs

between left right = string left *> manyTill anyChar (string right)

withPos action = do 
    p <- getPosition
    result <- action
    return (result, p)

parseTag = withPos $ do
    string "{%"
    skipMany space
    name <- identifier
    skipMany space
    rest <- manyTill anyChar (string "%}")
    return $ PTag name rest 

identifier = do 
    l <- choice [letter, oneOf "_"]
    s <- many (choice [alphaNum, oneOf "_"])
    return (l:s)

parseVar = withPos $ PVar <$> between "{{" "}}"

parseFile :: Parser -> String -> IO (Either Parsec.ParseError Template)
parseFile parser filename = do
    eitherTokens <- parseFromFile intermediateParser filename    
    return $ either Left (Parsec.Prim.runParser tokensToTemplate (parser, newTemplate) filename) eitherTokens

parseString :: Parser -> String -> Either Parsec.ParseError Template
parseString parser string =
    either Left
           (Parsec.Prim.runParser tokensToTemplate (parser, newTemplate) "")
         $ Parsec.Prim.runParser intermediateParser () "" string


tokensToTemplate :: Parsec [(Token, SourcePos)] ParserState Template 
tokensToTemplate = do 
    nodes <- catMaybes <$> many pNode 
    (_, t) <- getState
    return $ t {tmplNodes=nodes}

pNode = choice [pVar, pTag, pText]

pVar = do
    (PVar x, _) <- var 
    return $ Just $ Var $ strip x

pText = do
    (PText x, _) <- text
    return $ Just $ Text x 

pTag = do 
    ((PTag name rest), _) <- tag
    (parser, _) <- getState
    case lookup name (parserTagTypes parser) of 
        Nothing -> fail ("unknown tag: " ++ show name)
        Just (TagType t) -> t name rest

token' p = Parsec.Prim.token (show . fst) (snd) $ toMaybe $ p . fst
var = token' isVar
text = token' isText
tag = token' isTag
tagNamed name = token' (isTagNamed name)
tagNamedOneOf name = token' (isTagNamedOneOf name)

toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe f tokpos = if (f tokpos) then Just tokpos else Nothing

isVar :: Token -> Bool
isVar (PVar _) = True
isVar _ = False

isTag :: Token -> Bool
isTag (PTag _ _) = True
isTag _ = False

isTagNamed :: TagName -> Token -> Bool
isTagNamed aname tag = isTagNamedOneOf [aname] tag

isTagNamedOneOf names (PTag name _) = name `elem` names
isTagNamedOneOf _ _ = False

isText :: Token -> Bool
isText (PText _) = True
isText _ = False

strip :: String -> String
strip = f . f
    where f = reverse . dropWhile isSpace

failWithParseError :: (Parsec.Prim.Stream s m t) => Parsec.Error.ParseError -> Parsec.Prim.ParsecT s u m a
failWithParseError parseError = Parsec.Prim.mkPT $ 
    \_ -> return $ Parsec.Prim.Empty $ return $ Parsec.Prim.Error parseError

runSubParser parser state input = do
    name <- fmap sourceName getPosition
    either failWithParseError return $
      Parsec.Prim.runParser parser state name input

spaces = some space

runParseTagExpressions input = runSubParser parseTagExpressions () input
    where parseTagExpressions = do 
              optional spaces
              (choice [try pStr, try pVar]) `sepEndBy` spaces
          pStr = ExprStr <$> between "\"" "\""
          pVar = ExprVar <$> identifier


