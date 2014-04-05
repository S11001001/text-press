
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
import Data.Either (Either(..))
import Data.Function (on)
import Data.Functor ((<$>))
import Data.Map (fromList, Map, lookup, insert)
import Data.Maybe (catMaybes, listToMaybe)
import Prelude hiding (lookup)

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec.Error
import Text.Parsec.String (parseFromFile)
import Text.Parsec.Pos (SourcePos, sourceName)
import Text.Parsec.Prim (Parsec, getPosition, getState)
import qualified Text.Parsec.Prim as Parsec.Prim
import Text.Parser.Char (alphaNum, anyChar, letter, oneOf, space, string)
import Text.Parser.Combinators (choice, eof, many, manyTill, notFollowedBy, optional, sepEndBy, skipMany, some, try)
import Text.Parser.LookAhead (lookAhead)

import Text.Press.Types
import Text.Press.Render 

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
    where 

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
    (p, t) <- getState
    return $ t {tmplNodes=nodes}

pNode = choice [pVar, pTag, pText]

pVar = do
    (PVar x, pos) <- var 
    return $ Just $ Var $ strip x

pText = do
    (PText x, pos) <- text
    return $ Just $ Text x 

pTag = do 
    ((PTag name rest), pos) <- tag
    (parser, _) <- getState
    case lookup name (parserTagTypes parser) of 
        Nothing -> fail ("unknown tag: " ++ show name)
        Just (TagType t) -> t name rest

token' x = Parsec.Prim.token (show . fst) (snd) x
var = token' $ toMaybe $ isVar . fst
text = token' $ toMaybe $ isText . fst
tag = token' $ toMaybe $ isTag . fst
tagNamed name = token' $ toMaybe $ (isTagNamed name) . fst
tagNamedOneOf name = token' $ toMaybe $ (isTagNamedOneOf name) . fst

toMaybe f tokpos = if (f tokpos) then Just tokpos else Nothing

isVar (PVar _) = True
isVar otherwise = False

isTag (PTag _ _) = True
isTag otherwise = False

isTagNamed aname tag = isTagNamedOneOf [aname] tag

isTagNamedOneOf names (PTag name _) = name `elem` names
isTagNamedOneOf names otherwise = False

isText (PText _) = True
isText otherwise = False

strip = f . f
    where f = reverse . dropWhile isSpace

failWithParseError :: (Parsec.Prim.Stream s m t) => Parsec.Error.ParseError -> Parsec.Prim.ParsecT s u m a
failWithParseError parseError = Parsec.Prim.mkPT $ 
    \s -> return $ Parsec.Prim.Empty $ return $ Parsec.Prim.Error parseError

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


