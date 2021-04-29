module LExprParser where

import Grammar
import Lexer (alexScanTokens)
import Parser (parseExpr)

parseLExpr :: String -> LExpr
parseLExpr s = case parseExpr (alexScanTokens s) of
    Left err   -> error err
    Right expr -> expr

getExprFromInput :: IO (LExpr)
getExprFromInput = getContents >>= (return . parseLExpr)

