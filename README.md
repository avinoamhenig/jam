To run: `runhaskell -W -isrc Trns.Repl`

Then try: `.l example_programs/iszero`
(you can also run individual transformation script lines).

`.e` to eval.

`.q` to quit.

`.t $id|:exp` to show type.


cabal install readline --extra-include-dirs=/ser/local/Cellar/readline/6.3.8/include --extra-lib-dirs=/usr/local/Cellar/readline/6.3.8/lib --configure-option=--with-readline-includes=/usr/local/Cellar/readline/6.3.8/include/ --configure-option=--with-readline-libraries=/usr/local/Cellar/readline/6.3.8/lib/
