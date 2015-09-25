# Experimental Language

Experimental Language is a language created just to test some concepts.

Syntax
------

    ExprC := number
            | true
            | false
            | id
            | (if ExprC ExprC ExprC)
            | (ExprC op ExprC)
            | (fn (id ..) ExprC)
            | (ExprC [ExprC ..])


    op := + - * / | &

Usage
-----

./exprc-language myprogram.exprc

Compilation
-----------

ghc exprc-language.hs
