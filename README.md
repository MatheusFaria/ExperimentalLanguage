# Experimental Language

Experimental Language is a language created just to test some concepts.

Syntax
------

    ExprC := number
            | true
            | false
            | id
            | string
            | (if ExprC ExprC ExprC)
            | (ExprC op ExprC)
            | (fn (id ...) ExprC)
            | (ExprC [ExprC ...])
            | (where (id = ExprC, ...) ExprC)


    op := + - * / and or == <

Usage
-----

./exprc-language myprogram.exprc

Compilation
-----------

ghc exprc-language.hs
