import qualified Text.Parsec as Parsec
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import DataDefinitions

spaces :: Parser ()
spaces = Parsec.skipMany1 Parsec.space

parseOp :: Parser Char
parseOp = Parsec.oneOf "+-*/"

parseNum :: Parser ExprC
parseNum = do
    a <- (Parsec.many1 Parsec.digit)
    return (NumC (read a :: Float))

parseBinop :: Parser ExprC
parseBinop = do
    a <- parseNum
    op <- parseOp
    b <- parseNum
    return (BinOpC op a b)


-- Using:
-- Parsec.parse parseBinop "[source code]" "300+15"