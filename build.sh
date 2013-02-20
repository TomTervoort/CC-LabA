#!/bin/bash

# Builds the three components of the compiler.
# The argument indicates to the output directory. The current directory is used when it is not specified.

ghc --make -main-is ParseBibATerm ParseBibATerm.hs -outputdir $1 -o ./$1/parse-bib
ghc --make -main-is Bib2Html Bib2Html.hs -outputdir $1 -o ./$1/bib2html
ghc --make -main-is HTMLPrinter HTMLPrinter.hs -outputdir $1 -o ./$1/pp-html
