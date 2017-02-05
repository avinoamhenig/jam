module Jts.Parser (
  readJtsScript,
  readJtsCmd
) where

import Jts.Ast
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except
import Text.ParserCombinators.Parsec.Char (letter)

readJtsScript :: String -> Either ParseError JtsScript
readJtsScript input =
  case parse parseScript "JtsScript" input of
    Left err -> throwError err
    Right val -> return $ JtsScript val
  where parseScript = do skipMany emptyline
                         pJtsCmd `sepEndBy` (many1 emptyline)

readJtsCmd :: String -> Either ParseError JtsCmd
readJtsCmd input =
  case parse pJtsCmd "JtsCmd" input of
    Left err -> throwError err
    Right val -> return val

pJtsCmd :: Parser JtsCmd
pJtsCmd = pBndCmd <|> pRplCmd <|> pTypCmd

pBndCmd :: Parser JtsCmd
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

pRplCmd :: Parser JtsCmd
pRplCmd = do
  string "rpl"
  spaces
  replace <- pExpName
  spaces
  replaceWith <- pExpCreator
  maybeSpaces
  return $ RplCmd replace replaceWith

pTypCmd :: Parser JtsCmd
pTypCmd = do string "typ"
             tyDefDescs <- pTyDefDesc `sepBy1` (char '&')
             return $ TypCmd tyDefDescs

pTyDefDesc :: Parser TyDefDesc
pTyDefDesc = do
  spaces
  tyDefName <- pTyDefName
  spaces
  tyVarNames <- pTyVarName `sepEndBy` spaces
  char '='
  maybeSpaces
  consDefs <- pConsDef `sepBy1` (char '|' >> maybeSpaces)
  return $ TyDefDesc tyDefName tyVarNames consDefs

pName :: Parser String
pName = many $ noneOf " \t\n()"

pTyVarName :: Parser String
pTyVarName = do c <- letter
                s <- many alphaNum
                return $ c:s

pPrefixedName :: Char -> (String -> a) -> Parser a
pPrefixedName prefix constructor = do
  char prefix
  name <- pName
  return $ constructor name

pExpName :: Parser ExpName
pExpName = pPrefixedName ':' ExpName

pIdName :: Parser IdName
pIdName = pPrefixedName '$' IdName

pTyDefName :: Parser TyDefName
pTyDefName = pPrefixedName '#' TyDefName

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
             return $ CrNum $ read num
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

pConsDef :: Parser ConsDef
pConsDef = do
  idName <- pIdName
  maybeSpaces
  typeDescs <- pTypeDesc `sepEndBy` spaces
  return $ ConsDef idName typeDescs

pTypeDesc :: Parser TypeDesc
pTypeDesc = pTypeDefTypeDesc <|> pTypeVarTypeDesc
  where pTypeVarTypeDesc = pTyVarName >>=
                            (\tvn -> return $ TyVarTypeDesc tvn)
        pTypeDefTypeDesc = (pTyDefName >>=
                              (\tdn -> return $ TyDefTypeDesc tdn []))
                        <|> do
                          char '('
                          maybeSpaces
                          tyDefName <- pTyDefName
                          spaces
                          tyDescs <- pTypeDesc `sepBy` spaces
                          maybeSpaces
                          char ')'
                          return $ TyDefTypeDesc tyDefName tyDescs

spaces :: Parser ()
spaces = skipMany1 $ oneOf " \t"

maybeSpaces :: Parser ()
maybeSpaces = skipMany $ oneOf " \t"

maybeComment :: Parser ()
maybeComment = do
  skipMany1 $ string "--"
  skipMany  $ noneOf "\n"

emptyline :: Parser ()
emptyline = do
  skipMany $ oneOf " \t"
  optional maybeComment
  skipMany1 newline
