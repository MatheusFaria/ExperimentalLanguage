module Interpreter(interp) where

import Data.Maybe
import DataDefinitions

-- Sum two NumV and returns the resulting NumV
sumC :: ExprV -> ExprV -> ExprV
sumC (NumV a) (NumV b) = (NumV (a + b))

-- Maps the binary operation symbols to the actual implementation
binops = [('+', sumC)]

-- Given the symbol lookups for the binary operation in the table
getBinop :: Char -> (ExprV -> ExprV -> ExprV)
getBinop op
        | isNothing func = error "No symbol found"
        | isJust func = fromJust(func)
        where func = lookup op binops

-- Evaluates an Expression
interp :: ExprC -> ExprV
interp (NumC n) = (NumV n)
interp (TrueC) = (BoolV True)
interp (FalseC) = (BoolV False)
interp (BinOpC op a b) = ((getBinop op) (interp a) (interp b))

