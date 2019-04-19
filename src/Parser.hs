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
  , Tok.reservedOpNames = ["+", "-", "*", "==", "\\", ":", "->"]
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

identifier :: Parser String
identifier = Tok.identifier lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer

integer :: Parser Integer
integer = Tok.integer lexer

semi :: Parser String
semi = Tok.semi lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer


{- Parser -}


program :: Parser Program
program = do
  decls <- many decl
  entrypoint <- expr
  return $ Program decls entrypoint

decl :: Parser Decl
decl = do
  _ <- symbol "let"
  lhs <- identifier
  _ <- symbol "="
  rhs <- expr
  _ <- semi
  return (lhs, rhs)

-- includes application/operations, which have to be handled separately because of left-recursion
expr :: Parser CoreExpr
expr =  nonRecursiveExpr `chainl1` (return Apply)
    <|> nonRecursiveExpr `chainl1` (Op <$> operation)

nonRecursiveExpr :: Parser CoreExpr
nonRecursiveExpr =
        (parens expr)
    <|> (Var <$> identifier)
    <|> termLiteral
    <|> lambdaExpr
    <|> letExpr
    <|> ifExpr
    <|> fixExpr

lambdaExpr :: Parser CoreExpr
lambdaExpr = do
  _ <- symbol "\\"
  argName <- identifier
  _ <- symbol ":"
  argType <- typeExpr
  _ <- symbol "->"
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
--        <|> typeFunction

typeFunction :: Parser Type
typeFunction = do
  func <- typeExpr
  arg <- typeExpr
  return $ TFunction func arg

typeLiteral :: Parser String
typeLiteral =  symbol "Int"
           <|> symbol "Bool"

termLiteral :: Parser CoreExpr
termLiteral = Lit <$> (  (LInt <$> integer)
                 <|> (symbol "true" >> (return $ LBool True))
                 <|> (symbol "false" >> (return $ LBool False))
                  )
