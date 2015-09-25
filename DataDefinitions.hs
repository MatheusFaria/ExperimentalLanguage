{-
                Syntax
ExprC := number
        | true
        | false
        | id
        | (if ExprC ExprC ExprC)
        | (ExprC op ExprC)
        | (fn (id ..) ExprC)
        | (ExprC [ExprC ..])


op := + - * / | &
-}
module DataDefinitions(
                ExprC(..),
                ExprV(..),
                Binding(..),
                Environment) where

data ExprC =   NumC Float
             | TrueC
             | FalseC
             | IdC String
             | IfC ExprC ExprC ExprC
             | BinOpC String ExprC ExprC
             | LamC [String] ExprC
             | CallC ExprC [ExprC]
             deriving (Show, Eq)

data ExprV =   NumV {num :: Float}
             | BoolV {bool :: Bool}
             | CloV { params :: [String], fn :: ExprC, environment :: Environment }
             deriving (Show, Eq)

data Binding =  Bind {identifier :: String, value :: ExprV}
                deriving (Show, Eq)

type Environment = [Binding]
