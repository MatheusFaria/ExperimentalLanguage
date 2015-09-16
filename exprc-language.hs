data ExprC =   NumC Float
             | TrueC
             | FalseC
             | BinOpC ExprC ExprC Char
             deriving (Show)
