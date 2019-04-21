module Parser where

import Text.Parsec
import qualified Text.Parsec.Token as Tok

import Syntax

type Parser a = Parsec String () a


{- Lexer -}


langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = ""
  , Tok.commentEnd      = ""
  , Tok.commentLine     = ""
  , Tok.nestedComments  = False
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum
  , Tok.opStart         = oneOf "+-*="
  , Tok.opLetter        = oneOf "="
  , Tok.reservedNames   = ["let", "in", "fix", "true", "false", "if", "then", "else", "Int", "Bool"]
  , Tok.reservedOpNames = ["+", "-", "*", "==", "\\", ":", "."]
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

angles :: Parser a -> Parser a
angles = Tok.angles lexer

identifier :: Parser String
identifier = Tok.identifier lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer

integer :: Parser Integer
integer = Tok.integer lexer


{- Parser -}


program :: Parser Program
program = do
  decls <- many decl
  entrypoint <- expr
  _ <- eof
  return $ Program decls entrypoint

decl :: Parser Decl
decl = do
  _ <- symbol "let"
  lhs <- identifier
  _ <- symbol "="
  rhs <- expr
  _ <- symbol ";"
  return (lhs, rhs)

expr :: Parser CoreExpr
expr =  applyExpr `chainl1` (Op <$> (angles operation))

applyExpr :: Parser CoreExpr
applyExpr = nonLeftRecursiveExpr `chainl1` (return Apply)

nonLeftRecursiveExpr :: Parser CoreExpr
nonLeftRecursiveExpr =
    (Var <$> identifier)
    <|> termLiteral
    <|> lambdaExpr
    <|> letExpr
    <|> ifExpr
    <|> fixExpr
    <|> parens expr

lambdaExpr :: Parser CoreExpr
lambdaExpr = do
  _ <- symbol "\\"
  argName <- identifier
  _ <- symbol ":"
  argType <- typeExpr
  _ <- symbol "."
  body <- expr
  return $ Lambda argName argType body

letExpr :: Parser CoreExpr
letExpr = do
  _ <- symbol "let"
  lhs <- identifier
  _ <- symbol "="
  rhs <- expr
  _ <- symbol "in"
  body <- expr
  return $ Let lhs rhs body

ifExpr :: Parser CoreExpr
ifExpr = do
  _ <- symbol "if"
  cond <- expr
  _ <- symbol "then"
  trueExpr <- expr
  _ <- symbol "else"
  falseExpr <- expr
  return $ If cond trueExpr falseExpr

operation :: Parser BinOp
operation =  (symbol "+" >> return Add)
         <|> (symbol "-" >> return Subtract)
         <|> (symbol "*" >> return Multiply)
         <|> (symbol "==" >> return Equals)

fixExpr :: Parser CoreExpr
fixExpr = do
  _ <- symbol "fix"
  fixedExpr <- expr
  return $ Fix fixedExpr

typeExpr :: Parser Type
typeExpr =  TVariable <$> identifier
        <|> TConstructor <$> typeLiteral
        <|> typeFunction

typeFunction :: Parser Type
typeFunction = typeExpr `chainl1` (return TFunction)
            <|> parens typeExpr

typeLiteral :: Parser String
typeLiteral =  symbol "Int"
           <|> symbol "Bool"

termLiteral :: Parser CoreExpr
termLiteral =  Lit <$> (  integerLiteral
                      <|> trueLiteral
                      <|> falseLiteral
                       )

integerLiteral :: Parser Literal
integerLiteral = LInt <$> integer

trueLiteral :: Parser Literal
trueLiteral = do
  _ <- symbol "true"
  return $ LBool True

falseLiteral :: Parser Literal
falseLiteral = do
  _ <- symbol "false"
  return $ LBool False
