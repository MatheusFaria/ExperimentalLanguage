import qualified Text.Parsec as Parsec
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import DataDefinitions

spaces :: Parser ()
spaces = Parsec.skipMany Parsec.space

parseOp :: Parser Char
parseOp = Parsec.oneOf "+-*/&|"

parseNum :: Parser ExprC
parseNum = do
    a <- (Parsec.many1 Parsec.digit)
    return (NumC (read a :: Float))

parseBinop :: Parser ExprC
parseBinop = do
    Parsec.oneOf "("
    a <- parseAll
    op <- parseOp
    b <- parseAll
    Parsec.oneOf ")"
    return (BinOpC op a b)


parseAll :: Parser ExprC
parseAll = try parseBinop
		<|> parseNum

-- Using:
-- Parsec.parse parseBinop "[source code]" "300+15"
