#!/bin/bash

# Het argument is de output directory.

ghc --make -main-is ParseBibATerm ParseBibATerm.hs -outputdir $1 -o ./$1/parse-bib
# TODO: 
ghc --make -main-is HTMLPrinter HTMLPrinter.hs -outputdir $1 -o ./$1/pp-html
