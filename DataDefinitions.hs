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
                ExprC(NumC, TrueC, FalseC, BinOpC, IfC, LamC, CallC),
                ExprV(..),
                Binding(Bind)) where

data ExprC =   NumC Float
             | TrueC
             | FalseC
             | IfC ExprC ExprC ExprC
             | BinOpC String ExprC ExprC
             | LamC [String] ExprC
             | CallC ExprC [ExprC]
             deriving (Show, Eq)

data ExprV =   NumV Float
             | BoolV Bool
             | CloV { params :: [String], f :: ExprC, e :: [Binding] }
             deriving (Show, Eq)

data Binding =  Bind String ExprV
                deriving (Show, Eq)