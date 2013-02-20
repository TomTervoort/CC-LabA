#!/bin/bash

# After building, this runs the three components after each other. parse-bib will read from stdin and pp-html will send
# its output to stdout.
# The argument indicates to the output directory. The current directory is used when it is not specified.

./$1/parse-bib | ./$1/bib2html | ./$1/pp-html
