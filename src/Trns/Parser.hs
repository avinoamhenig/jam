module Trns.Parser (
  readTrnsScript,
  readTrnsCmd
) where

import Trns.AST
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except

readTrnsScript :: String -> Either ParseError TrnsScript
readTrnsScript input =
  case parse parseScript "TrnsScript" input of
    Left err -> throwError err
    Right val -> return $ TrnsScript val
  where parseScript = do skipMany emptyline
                         pTrnsCmd `sepEndBy` (many1 emptyline)

readTrnsCmd :: String -> Either ParseError TrnsCmd
readTrnsCmd input =
  case parse pTrnsCmd "TrnsCmd" input of
    Left err -> throwError err
    Right val -> return val

pTrnsCmd :: Parser TrnsCmd
pTrnsCmd = pBndCmd <|> pRplCmd

pBndCmd :: Parser TrnsCmd
pBndCmd = do
  string "bnd"
  spaces
  scope <- pExpName
  spaces
  idName <- pIdName
  spaces
  valName <- pExpName
  maybeSpaces
  return $ BndCmd scope idName valName

pRplCmd :: Parser TrnsCmd
pRplCmd = do
  string "rpl"
  spaces
  replace <- pExpName
  spaces
  replaceWith <- pExpCreator
  maybeSpaces
  return $ RplCmd replace replaceWith

pName :: Char -> (String -> a) -> Parser a
pName prefix constructor = do
  char prefix
  name <- many $ noneOf " \t\n()"
  return $ constructor name

pExpName :: Parser ExpName
pExpName = pName ':' ExpName

pIdName :: Parser IdName
pIdName = pName '$' IdName

pExpCreator :: Parser ExpCreator
pExpCreator =  try pUnit
           <|> try pNum
           <|> try pLambda
           <|> try pIdExp
           <|> try pApp
           <|> try pIf

pUnit =   do string "Unit("
             maybeSpaces
             char ')'
             return CrUnit
pNum =    do string "Num("
             maybeSpaces
             num <- many1 digit
             maybeSpaces
             char ')'
             return $ CrNum $ toInteger $ read num
pLambda = do string "Lambda("
             maybeSpaces
             argName <- pIdName
             spaces
             bodyName <- pExpName
             maybeSpaces
             char ')'
             return $ CrLambda argName bodyName
pIdExp =  do string "IdExp("
             maybeSpaces
             idName <- pIdName
             maybeSpaces
             char ')'
             return $ CrIdExp idName
pApp =    do string "App("
             maybeSpaces
             f <- pExpName
             spaces
             x <- pExpName
             maybeSpaces
             char ')'
             return $ CrApp f x
pIf =     do string "If("
             maybeSpaces
             c <- pExpName
             spaces
             t <- pExpName
             spaces
             e <- pExpName
             maybeSpaces
             char ')'
             return $ CrIf c t e

spaces :: Parser ()
spaces = skipMany1 $ oneOf " \t"

maybeSpaces :: Parser ()
maybeSpaces = skipMany $ oneOf " \t"

maybeComment :: Parser ()
maybeComment = do
  skipMany1 $ char '#'
  skipMany  $ noneOf "\n"

emptyline :: Parser ()
emptyline = do
  skipMany $ oneOf " \t"
  optional maybeComment
  skipMany1 newline
