{-# LANGUAGE NoImplicitPrelude #-}

module Parser where

import Protolude hiding ((<|>), try, Type, many)
import Prelude (words)

import Text.Parsec hiding (try)
import Text.Parsec.String
import Text.Parsec.Language
import Text.ParserCombinators.Parsec hiding (spaces, integer)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Token

import Control.Applicative hiding (many, (<|>))
import Data.Functor.Identity
import System.IO

--import qualified Data.Text as Text (words)

import Expr

---------------------------------------------------------------------------
-- LEXER
---------------------------------------------------------------------------

ops :: [[Char]]
ops = words "+ - * ; : ( ) = AND OR XOR ->"

keyWords :: [[Char]]
keyWords = words "true false if then else let in bool int function fun"

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser (emptyDef {
             Token.identStart = letter
            , Token.identLetter = alphaNum <|> letter
            , Token.reservedNames = keyWords
            , Token.reservedOpNames = ops
            , Token.commentStart = "##"
            , Token.commentEnd = "##"
        })

reserved :: [Char] -> Parser ()
reserved = Token.reserved lexer 

reservedOp :: [Char] -> Parser ()
reservedOp = Token.reservedOp lexer

---------------------------------------------------------------------------
-- PARSER
---------------------------------------------------------------------------

binaryOp :: [Char] -> Ex.Assoc -> (Ex.Operator [Char] () Data.Functor.Identity.Identity Expr)
binaryOp name = Ex.Infix $ reservedOp name *> return (makeBinOp name)

funApply :: Ex.Operator [Char] () Data.Functor.Identity.Identity Expr
funApply = Ex.Infix space Ex.AssocLeft
  where 
    space :: Parser (Expr -> Expr -> Expr)
    space = EFunApp <$ whitespace <* notFollowedBy (choice . map reservedOp $ ops) 

opTable :: Ex.OperatorTable [Char] () Data.Functor.Identity.Identity Expr
opTable = [ [funApply]
          , [ binaryOp "*" Ex.AssocLeft ]
          , [ binaryOp "+" Ex.AssocLeft
            , binaryOp "-" Ex.AssocLeft 
            ]
          , [ binaryOp "AND" Ex.AssocLeft 
            , binaryOp "OR" Ex.AssocLeft
            , binaryOp "XOR" Ex.AssocLeft 
            ]
          ]

whitespace :: Parser ()
whitespace = Token.whiteSpace lexer -- parses whitespace

boolExpr :: Parser Expr
boolExpr =  EBool True <$ (try $ reserved "true")
        <|> EBool False <$ (try $ reserved "false")

intExpr :: Parser Expr
intExpr = EInt <$> Token.integer lexer

varExpr :: Parser Expr
varExpr = EVar <$> Token.identifier lexer

condExpr :: Parser Expr
condExpr = ECond <$> try b <*> e1 <*> e2
  where
    b = reserved "if" *> expr
    e1 = reserved "then" *> expr
    e2 = reserved "else" *> expr

letTAExpr :: Parser Expr
letTAExpr = do
    name <- reserved "let" *> Token.identifier lexer
    typ <- reservedOp ":" *> parseType
    e1 <- reservedOp "=" *> expr
    e2 <- reserved "in" *> expr
    return $ ELetTA name typ e1 e2

letExpr :: Parser Expr
letExpr = do
    name <- reserved "let" *> Token.identifier lexer
    e1 <- reservedOp "=" *> expr
    e2 <- reserved "in" *> expr
    return $ ELet name e1 e2

parseType :: Parser Type
parseType = TBool <$ reserved "bool"
         <|> TInt <$ reserved "int"
         <|> TFun <$> (reserved "function" *> parseType) <*> parseType 

funTAExpr :: Parser Expr
funTAExpr = do
    varName <- reserved "fun" 
               *> reservedOp "(" 
               *> Token.identifier lexer
    varType <- reservedOp ":" *> parseType
    returnType <- reservedOp ")" *> reservedOp ":" *> parseType
    e <- reservedOp "=" *> expr
    return $ EFunTA varName varType returnType e

funExpr :: Parser Expr
funExpr = do
    varName <- reserved "fun" *> Token.identifier lexer
    e <- reservedOp "=" *> expr
    return $ EFun varName e

funApplyExpr :: Parser Expr
funApplyExpr = do
    funName <- varExpr
    arg <- whitespace *> expr
    return $ EFunApp funName arg

-- parseTypeAnnotation :: Parser (Maybe Type)
-- parseTypeAnnotation = do
--     e <- expr
--     t <- reservedOp ":" *> parseType

expr :: Parser Expr
expr = Ex.buildExpressionParser opTable 
       $ funExpr
      <|> funTAExpr
      <|> letExpr 
      <|> letTAExpr
      <|> condExpr 
      <|> varExpr 
      <|> boolExpr 
      <|> intExpr 

-- parses expression without semicolon
parseExpr :: [Char] -> Either ParseError Expr
parseExpr str = parse expr "<stdin>" str

parseTypeTester :: [Char] -> Either ParseError Type
parseTypeTester str = parse parseType "<stdin>" str


-- parseMany :: [Char] -> Either ParseError [Expr]
-- parseMany str = parse (many $ expr <* reservedOp ";") "<stdin>" str


