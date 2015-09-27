{-
                Syntax
ExprC := number
        | true
        | false
        | id
        | string
        | (if ExprC ExprC ExprC)
        | (ExprC op ExprC)
        | (fn (id ...) ExprC)
        | (ExprC [ExprC ...])
        | (where (id = ExprC) ... ExprC)


op := + - * / and or ==
-}
module DataDefinitions(
                ExprC(..),
                ExprV(..),
                Binding(..),
                Environment,
                availableBinops,
                ) where

data ExprC =   NumC Float
             | TrueC
             | FalseC
             | StringC String
             | IdC String
             | IfC ExprC ExprC ExprC
             | BinOpC String ExprC ExprC
             | LamC [String] ExprC
             | CallC ExprC [ExprC]
             deriving (Show, Eq)

data ExprV =   NumV {num :: Float}
             | BoolV {bool :: Bool}
             | StringV {str :: String}
             | CloV { params :: [String], fn :: ExprC, environment :: Environment }
             deriving (Show, Eq)

data Binding =  Bind {identifier :: String, value :: ExprV}
                deriving (Show, Eq)

type Environment = [Binding]


availableBinops = ["+", "-", "*", "/", "and", "or", "=="]
