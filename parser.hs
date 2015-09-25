module Parser(mainParse) where

import qualified Text.Parsec as Parsec
import Text.ParserCombinators.Parsec
import DataDefinitions


parseOp :: Parser String
parseOp = choice (map string ["-", "+", "/", "*", "&", "|"])

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

idSeparator = spaces
idPattern = many1 (Parsec.choice [letter, digit, noneOf "+-*/&|()=, "])

parseId :: Parser ExprC
parseId = do
        identifier <- idPattern
        return (IdC identifier)

parseBinop :: Parser ExprC
parseBinop = do
    Parsec.oneOf "(" >> spaces
    left <- parseAll
    op <- spaces >> parseOp
    right <- spaces >> parseAll
    spaces >> Parsec.oneOf ")"
    return (BinOpC op left right)

parseIf :: Parser ExprC
parseIf = do
    Parsec.oneOf "(" >> spaces
    Parsec.string "if" >> spaces
    cond <- parseAll
    ifTrue <- spaces >> parseAll
    ifFalse <- spaces >> parseAll
    spaces >> Parsec.oneOf ")"
    return (IfC cond ifTrue ifFalse)

parseFunction :: Parser ExprC
parseFunction = do
    Parsec.oneOf "(" >> spaces
    Parsec.string "fn" >> spaces
    Parsec.oneOf "(" >> spaces
    params <- sepBy idPattern idSeparator
    spaces >> Parsec.oneOf ")"
    body <- spaces >> parseAll
    spaces >> Parsec.oneOf ")"
    return (LamC params body)

parseCall :: Parser ExprC
parseCall = do
    Parsec.oneOf "(" >> spaces
    f <- spaces >> parseAll
    values <- spaces >> sepBy parseAll idSeparator
    spaces >> Parsec.oneOf ")"
    return (CallC f values)


parseAll :: Parser ExprC
parseAll = try parseNum
        <|> parseTrue
        <|> parseFalse
        <|> try parseId
        <|> try parseBinop
        <|> try parseIf
        <|> try parseFunction
        <|> try parseCall


mainParse :: String -> ExprC
mainParse input = case (Parsec.parse parseAll "[source code]" input) of
    Right expr -> expr
    Left err -> error "Invalid Syntax"
