module Parser(mainParse) where

import qualified Text.Parsec as Parsec
import Text.ParserCombinators.Parsec
import DataDefinitions


parseOp :: Parser Char
parseOp = Parsec.oneOf "+-*/&|"

parseNum :: Parser ExprC
parseNum = do
    a <- (Parsec.many1 Parsec.digit)
    return (NumC (read a :: Float))

parseTrue :: Parser ExprC
parseTrue = do
    Parsec.string "true"
    return (TrueC)

parseFalse :: Parser ExprC
parseFalse = do
    Parsec.string "false"
    return (FalseC)

parseBinop :: Parser ExprC
parseBinop = do
    Parsec.oneOf "(" >> spaces
    a <- spaces >> parseAll
    op <- spaces >> parseOp
    b <- spaces >> parseAll
    spaces >> Parsec.oneOf ")"
    return (BinOpC op a b)

parseAll :: Parser ExprC
parseAll = try parseNum
        <|> parseTrue
        <|> parseFalse
        <|> parseBinop


mainParse :: String -> ExprC
mainParse input = case (Parsec.parse parseAll "[source code]" input) of
    Right expr -> expr
    Left err -> error "Invalid Syntax"
