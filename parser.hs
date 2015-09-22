import qualified Text.Parsec as Parsec
import Text.ParserCombinators.Parsec
import DataDefinitions

spaces :: Parser ()
spaces = Parsec.skipMany Parsec.space

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
    Parsec.oneOf "("
    a <- parseAll
    op <- parseOp
    b <- parseAll
    Parsec.oneOf ")"
    return (BinOpC op a b)


parseAll :: Parser ExprC
parseAll = try parseNum
        <|> parseTrue
        <|> parseFalse
        <|> parseBinop

-- Using:
-- Parsec.parse parseBinop "[source code]" "300+15"
