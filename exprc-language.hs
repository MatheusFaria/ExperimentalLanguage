import Data.Maybe

data ExprC =   NumC Float
             | TrueC
             | FalseC
             | BinOpC Char ExprC ExprC
             deriving (Show)

data ExprV =   NumV Float
             | BoolV Bool
             deriving (Show)


sumC :: ExprV -> ExprV -> ExprV
sumC (NumV a) (NumV b) = (NumV (a + b))

binops = [('+', sumC)]

getBinop :: Char -> (ExprV -> ExprV -> ExprV)
getBinop op
        | isNothing func = error "No symbol found"
        | isJust func = fromJust(func)
        where func = lookup op binops

interp :: ExprC -> ExprV
interp (NumC n) = (NumV n)
interp (TrueC) = (BoolV True)
interp (FalseC) = (BoolV False)
interp (BinOpC op a b) = ((getBinop op) (interp a) (interp b))

