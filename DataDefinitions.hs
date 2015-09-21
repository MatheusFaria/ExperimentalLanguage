module DataDefinitions(
                ExprC(NumC, TrueC, FalseC, BinOpC),
                ExprV(NumV, BoolV)) where

data ExprC =   NumC Float
             | TrueC
             | FalseC
             | BinOpC Char ExprC ExprC
             deriving (Show)

data ExprV =   NumV Float
             | BoolV Bool
             deriving (Show)

