{-
                Syntax
ExprC := number
        | true
        | false
        | (if ExprC ExprC ExprC)
        | (op ExprC ExprC)


op := + - * / | &
-}
module DataDefinitions(
                ExprC(NumC, TrueC, FalseC, BinOpC, IfC, FunC),
                ExprV(NumV, BoolV)) where

data ExprC =   NumC Float
             | TrueC
             | FalseC
             | IfC ExprC ExprC ExprC
             | BinOpC String ExprC ExprC
             | LamC [String] ExprC
             deriving (Show, Eq)

data ExprV =   NumV Float
             | BoolV Bool
             | ClosV [String] ExprC
             deriving (Show, Eq)

