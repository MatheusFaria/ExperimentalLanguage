import System.Environment
import System.Exit

import Interpreter

main = getArgs >>= parseInput >>= eval

-- XXX: stub for eval
eval a = putStr a

parseInput ["-h"] = usage   >> exit
parseInput ["-v"] = version >> exit
parseInput []     = getContents
parseInput (h:t)  = readFile h

usage   = putStrLn "Usage: exprc-language [-vh] [file]"
version = putStrLn "Experimental Language v0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
