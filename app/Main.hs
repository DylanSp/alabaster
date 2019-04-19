module Main where

import Data.List
import Text.Parsec

import Parser

import Eval
import Syntax
import Type

main :: IO ()
main = do
  putStrLn "Input:"
  input <- getLine
  let prog = runParse input
  putStrLn $ "Parsed"
  let typeChecked = typeCheck <$> prog
  putStrLn $ "Typecheck result: " ++ (showEither typeChecked)
  let evalResult = evalProgram <$> prog
  putStrLn $ "Eval result: " ++ show evalResult
  putStrLn "done"

runParse :: String -> Either String Program
runParse input = case (parse program "" input) of
  Left err -> Left $ show err
  Right ast -> Right ast

typeCheck :: Program -> String
typeCheck (Program decls entrypoint) = (unwords $ intersperse "\n" (checkDecl <$> decls)) ++ "\n" ++ showEither (typeOf entrypoint)

checkDecl :: Decl -> String
checkDecl (ident, expression) = "Type of " ++ ident ++ " is " ++ (show (typeOf expression))

showEither :: (Show a, Show b) => Either a b -> String
showEither (Left str) = show str
showEither (Right str) = show str 

{-
typeCheck :: CoreExpr -> Either String String
typeCheck ast = case (typeOf ast) of
  Left typeErr -> show typeErr
  Right rootType -> show rootType
-}


fromRight (Right (Program _ entry)) = entry

prog1Text = "\\x : a -> x"

prog2Text = "(\\x : a -> x) 1"
