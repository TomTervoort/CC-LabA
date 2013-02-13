#!/bin/bash

ghc --make -main-is ParseBibATerm ParseBibATerm.hs -outputdir $1 -o ./$1/parse-bib

# TODO ...
