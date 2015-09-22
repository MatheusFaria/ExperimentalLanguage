module Interpreter(interp) where

import Data.Maybe
import DataDefinitions

-- ############ Binary Operations #############

-- Maps the binary operation symbols to the actual implementation
binops = [('+', sumC), ('-', subC), ('*', mulC), ('/', divC),
          ('&', andC), ('|', orC)]

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
getBinop :: Char -> (ExprV -> ExprV -> ExprV)
getBinop op
        | isNothing func = error ("Invalid Binary Operation " ++ (op : []))
        | isJust func = fromJust(func)
        where func = lookup op binops




-- ################### Evaluator ################

-- Evaluates an Expression
interp :: ExprC -> ExprV
interp (NumC n) = (NumV n)
interp (TrueC) = (BoolV True)
interp (FalseC) = (BoolV False)
interp (BinOpC op a b) = ((getBinop op) (interp a) (interp b))
interp (IfC tes ifTrue ifFalse)
        | (interp tes) == (BoolV True) = (interp ifTrue)
        | otherwise = (interp ifFalse)