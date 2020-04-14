#!/bin/csh -f
alias STDERR 'bash -c "cat - 1>&2"'

echo ============== running  clojure tests ==============

echo pwd= `pwd`
set tmpfile = clojure.$$.out
lein test |& tee $tmpfile
