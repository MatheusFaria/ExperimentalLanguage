module Parser(mainParse) where

import qualified Text.Parsec as Parsec
import Text.ParserCombinators.Parsec
import DataDefinitions
import Data.Char
import Data.List

-- ############## Parse Data Structure #############

data Expression = EmptyExpression
                | ExpressionElement String
                | ExpressionList [Expression]
                deriving(Show)


-- ############## Pre-Parse ####################

emptyExpr :: Parser Expression
emptyExpr = do
        char '(' >> spaces >> char ')'
        return EmptyExpression

elementExpr :: Parser Expression
elementExpr = do
        element <- many1 (satisfy (\x -> not(isSpace(x)) && x /= '(' && x /= ')' && x /= ','))
        return (ExpressionElement element)

enclosedExpr :: Parser Expression
enclosedExpr = do
    char '(' >> spaces
    element <- sepEndBy preParseRule (many1 (oneOf " ,"))
    spaces >> char ')'
    return (ExpressionList element)

preParseRule :: Parser Expression
preParseRule = try enclosedExpr
           <|> try elementExpr
           <|> try emptyExpr


preParse :: String -> Expression
preParse input = case (Parsec.parse preParseRule "[source code]" input) of
    Right expr -> expr
    Left err -> error ("Invalid Syntax: " ++ input)


expjoin :: Expression -> String
expjoin EmptyExpression = ""
expjoin (ExpressionElement s) = s ++ " "
expjoin (ExpressionList s) = "( " ++ (foldr (++) [] (map expjoin s)) ++ ")"


-- ############### Patterns ####################


idSeparator = spaces

idPattern :: Parser String
idPattern = many1 (Parsec.choice [letter, digit, noneOf "+-*/&|()=, \""])

opPattern :: Parser String
opPattern = choice (map string availableBinops)


-- ############### Parsers ####################


-- Parse a String to NumC
parseNum :: Parser ExprC
parseNum = do
    a <- (Parsec.many1 Parsec.digit)
    return (NumC (read a :: Float))


-- Parse a String to TrueC
parseTrue :: Parser ExprC
parseTrue = do
    Parsec.string "true"
    return (TrueC)


-- Parse a String to FalseC
parseFalse :: Parser ExprC
parseFalse = do
    Parsec.string "false"
    return (FalseC)


-- Parse a String to IdC
parseId :: Parser ExprC
parseId = do
        identifier <- idPattern
        return (IdC identifier)


-- Parse a String to StringC
parseString :: Parser ExprC
parseString = do
        char '\"'
        str <- manyTill anyChar (char '\"')
        return (StringC str)


-- Parse a String to BinOpC
parseBinop :: Parser ExprC
parseBinop = do
    Parsec.oneOf "(" >> spaces
    left <- parseAll
    op <- spaces >> opPattern
    right <- spaces >> parseAll
    spaces >> Parsec.oneOf ")"
    return (BinOpC op left right)


-- Parse a String to IfC
parseIf :: Parser ExprC
parseIf = do
    Parsec.oneOf "(" >> spaces
    Parsec.string "if" >> spaces
    cond <- parseAll
    ifTrue <- spaces >> parseAll
    ifFalse <- spaces >> parseAll
    spaces >> Parsec.oneOf ")"
    return (IfC cond ifTrue ifFalse)


-- Parse a String to lamC
parseFunction :: Parser ExprC
parseFunction = do
    Parsec.oneOf "(" >> spaces
    Parsec.string "fn" >> spaces
    Parsec.oneOf "(" >> spaces
    params <- sepEndBy idPattern idSeparator
    spaces >> Parsec.oneOf ")"
    body <- spaces >> parseAll
    spaces >> Parsec.oneOf ")"
    return (LamC params body)


-- Parse a String to callC
parseCall :: Parser ExprC
parseCall = do
    Parsec.oneOf "(" >> spaces
    f <- spaces >> parseAll
    values <- spaces >> sepEndBy parseAll idSeparator
    spaces >> Parsec.oneOf ")"
    return (CallC f values)


-- Parse a string to a desugared where
parseWhereAttr :: Parsec.Parsec String () (String,String)
parseWhereAttr = do
    spaces
    id <- idPattern
    spaces >> Parsec.char '=' >> spaces
    value <- preParseRule
    spaces
    return (id, expjoin(value))

parseWhereAttrs :: Parsec.Parsec String () [(String,String)]
parseWhereAttrs = Parsec.sepEndBy parseWhereAttr (Parsec.char ',')

parseWhere :: Parser ExprC
parseWhere = do
    Parsec.oneOf "(" >> spaces
    Parsec.string "where" >> spaces
    Parsec.oneOf "(" >> spaces
    attrs <- parseWhereAttrs
    spaces >> Parsec.oneOf ")"
    body <- spaces >> preParseRule
    spaces >> Parsec.oneOf ")"
    return (mainParse ("((fn (" ++ (foldr (\x y -> x ++ " " ++ y) [] (map fst attrs)) ++ ") " ++ expjoin(body) ++ ")" ++ (foldr (++) [] (map snd attrs)) ++ ")"))


parseNamedFunction :: Parser ExprC
parseNamedFunction = do
    Parsec.oneOf "(" >> spaces
    Parsec.string "def" >> Parsec.many1 space
    name <- idPattern
    spaces >> Parsec.oneOf "(" >> spaces
    params <- sepEndBy idPattern idSeparator
    spaces >> Parsec.oneOf ")" >> spaces
    body <- preParseRule
    spaces >> Parsec.oneOf ")" >> spaces
    return (mainParse ("(where (" ++ name ++ "= (fn (" ++ (intercalate " " params) ++ ") " ++ expjoin(body) ++ ")) " ++ name ++ ")"))


-- ############## Main Parse ###################


parseAll :: Parser ExprC
parseAll = try parseNum
        <|> try parseTrue
        <|> try parseFalse
        <|> try parseId
        <|> try parseString
        <|> try parseBinop
        <|> try parseIf
        <|> try parseFunction
        <|> try parseNamedFunction
        <|> try parseCall
        <|> try parseWhere


mainParse :: String -> ExprC
mainParse input = case (Parsec.parse parseAll "[source code]" input) of
    Right expr -> expr
    Left err -> error ("Invalid Syntax: " ++ input)
