module Interpreter(interp) where

import Data.Maybe
import DataDefinitions

-- ############ Binary Operations #############

-- Maps the binary operation symbols to the actual implementation
binops = [("+", sumC), ("-", subC), ("*", mulC), ("/", divC),
          ("and", andC), ("or", orC), ("==", eqC), ("<", lessC)]

-- Sum two NumV and returns the resulting NumV
sumC :: ExprV -> ExprV -> ExprV
sumC (NumV a) (NumV b) = (NumV (a + b))
sumC (StringV a) (StringV b) = (StringV (a ++ b))
sumC _ _ = error "Operation + not define for this data type"

-- Subtracts two NumV and returns the resulting NumV
subC :: ExprV -> ExprV -> ExprV
subC (NumV a) (NumV b) = (NumV (a - b))
subC _ _ = error "Operation - not define for this data type"

-- Multiply two NumV and returns the resulting NumV
mulC :: ExprV -> ExprV -> ExprV
mulC (NumV a) (NumV b) = (NumV (a * b))
mulC _ _ = error "Operation * not define for this data type"

-- Divides two NumV and returns the resulting NumV
divC :: ExprV -> ExprV -> ExprV
divC (NumV a) (NumV b)
        | b == 0 = error "Division by zero"
        | otherwise = (NumV (a / b))
divC _ _ = error "Operation / not define for this data type"

-- Does the logic operation AND
andC :: ExprV -> ExprV -> ExprV
andC (BoolV a) (BoolV b) = (BoolV (a && b))
andC _ _ = error "Operation 'and' not define for this data type"

-- Does the logic operation OR
orC :: ExprV -> ExprV -> ExprV
orC (BoolV a) (BoolV b) = (BoolV (a || b))
orC _ _ = error "Operation 'or' not define for this data type"

-- Does the logic operation ==
eqC :: ExprV -> ExprV -> ExprV
eqC (NumV a)    (NumV b)    = (BoolV (a == b))
eqC (BoolV a)   (BoolV b)   = (BoolV (a == b))
eqC (StringV a) (StringV b) = (BoolV (a == b))
eqC _ _ = BoolV False

-- Does the logic operation <
lessC :: ExprV -> ExprV -> ExprV
lessC (NumV a) (NumV b)  = (BoolV (a < b))
lessC _ _ = error "Operation '<' not define for this data type"

-- Given the symbol lookups for the binary operation in the table
getBinop :: String -> (ExprV -> ExprV -> ExprV)
getBinop op
        | isNothing func = error ("Invalid Binary Operation " ++ op)
        | isJust func = fromJust(func)
        where func = lookup op binops


-- ################# Environment Operations #################

-- looks for a identifier inside a environment
lookup_in_env :: String -> Environment -> ExprV
lookup_in_env x (h:t)
        | x == (identifier h) = (value h)
        | null t = error("Id [" ++ x ++ "] out of scope")
        | otherwise = lookup_in_env x t


-- Add to a environment all the bindings related to a funciton call
generate_function_env :: [String] -> [ExprV] -> Environment -> Environment
generate_function_env params values env
        | length params /= length values = error "Wrong number of parameters on function call"
        | null params = env
        | otherwise = (Bind (head params) (head values)) : (generate_function_env (tail params) (tail values) env)


-- ################### Evaluator ################

-- Evaluates an Expression
interp :: ExprC -> Environment -> ExprV
interp (NumC n) env = (NumV n)
interp (TrueC) env = (BoolV True)
interp (FalseC) env = (BoolV False)
interp (StringC s) env = (StringV s)
interp (IdC x) env = lookup_in_env x env
interp (BinOpC op a b) env = ((getBinop op) (interp a env) (interp b env))
interp (IfC tes ifTrue ifFalse) env
        | (interp tes env) == (BoolV True) = (interp ifTrue env)
        | otherwise = (interp ifFalse env)
interp (LamC args body) env = (CloV args body env)
interp (CallC func args)  env = (let fd = (interp func env) in (interp (fn fd) ((generate_function_env (params fd) (map (\x -> (interp x env)) args) env) ++ (environment fd))))
