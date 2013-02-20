#!/bin/bash

# Het argument is de output directory.

ghc --make -main-is ParseBibATerm ParseBibATerm.hs -outputdir $1 -o ./$1/parse-bib
ghc --make -main-is Bib2Html Bib2Html.hs -outputdir $1 -o ./$1/bib2html
ghc --make -main-is HTMLPrinter HTMLPrinter.hs -outputdir $1 -o ./$1/pp-html
