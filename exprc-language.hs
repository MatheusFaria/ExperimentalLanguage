import System.Environment
import System.Exit

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
eval a = print (prettyPrintEval (interp (mainParse a) []))
-- eval a = print (mainParse a)

prettyPrintEval :: ExprV -> String
prettyPrintEval (NumV n) = show n

prettyPrintEval (CloV _ _ _) = "function"

prettyPrintEval (BoolV t)
        | t = "true"
        | otherwise = "false"
