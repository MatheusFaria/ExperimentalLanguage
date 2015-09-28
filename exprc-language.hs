import System.Environment
import System.Exit
import Data.Char

import Interpreter
import Parser
import DataDefinitions

main = getArgs >>= parseInput >>= eval

parseInput ["-h"] = usage   >> exit
parseInput ["-v"] = version >> exit
parseInput []     = getContents
parseInput (h:t)  = readFile h

usage   = putStrLn "Usage: exprc-language [-vh] [file]"
version = putStrLn "Experimental Language v0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)




-- Parses and evalutates an expression
eval :: String -> IO()
eval a = print (prettyPrintEval (interp (mainParse (replace "\n" " " a)) []))

prettyPrintEval :: ExprV -> String
prettyPrintEval (NumV n) = show n

prettyPrintEval (CloV _ _ _) = "function"

prettyPrintEval (BoolV t)
        | t = "true"
        | otherwise = "false"

prettyPrintEval (StringV t) = t

replace :: String -> String -> String -> String
replace a b str
    | null str = ""
    | ((head str) : []) == a = b ++ (replace a b (tail str))
    | otherwise = (head str) : (replace a b (tail str))

strip :: String -> String
strip str
    | null str = ""
    | (isSpace (head str)) = (strip (tail str))
    | (isSpace (last str)) = (strip (init str))
    | otherwise = str
