module Interpreter(interp) where

import Data.Maybe
import DataDefinitions

-- ############ Binary Operations #############

-- Maps the binary operation symbols to the actual implementation
binops = [("+", sumC), ("-", subC), ("*", mulC), ("/", divC),
          ("&", andC), ("|", orC)]

-- Sum two NumV and returns the resulting NumV
sumC :: ExprV -> ExprV -> ExprV
sumC (NumV a) (NumV b) = (NumV (a + b))

-- Subtracts two NumV and returns the resulting NumV
subC :: ExprV -> ExprV -> ExprV
subC (NumV a) (NumV b) = (NumV (a - b))

-- Multiply two NumV and returns the resulting NumV
mulC :: ExprV -> ExprV -> ExprV
mulC (NumV a) (NumV b) = (NumV (a * b))

-- Divides two NumV and returns the resulting NumV
divC :: ExprV -> ExprV -> ExprV
divC (NumV a) (NumV b)
        | b == 0 = error "Division by zero"
        | otherwise = (NumV (a / b))

-- Does the logic operation AND
andC :: ExprV -> ExprV -> ExprV
andC (BoolV a) (BoolV b) = (BoolV (a && b))

-- Does the logic operation OR
orC :: ExprV -> ExprV -> ExprV
orC (BoolV a) (BoolV b) = (BoolV (a || b))


-- Given the symbol lookups for the binary operation in the table
getBinop :: String -> (ExprV -> ExprV -> ExprV)
getBinop op
        | isNothing func = error ("Invalid Binary Operation " ++ op)
        | isJust func = fromJust(func)
        where func = lookup op binops




-- ################### Evaluator ################

-- Evaluates an Expression
interp :: ExprC -> [Binding] -> ExprV
interp (NumC n) env = (NumV n)
interp (TrueC) env = (BoolV True)
interp (FalseC) env = (BoolV False)
interp (BinOpC op a b) env = ((getBinop op) (interp a env) (interp b env))
interp (IfC tes ifTrue ifFalse) env
        | (interp tes env) == (BoolV True) = (interp ifTrue env)
        | otherwise = (interp ifFalse env)
interp (FunC args body) env = (CloV args body env)
