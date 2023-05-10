#!/bin/sh
# Note: the fourth pattern is not a joke. h is the pattern, and (i) is
# the command (in that case, a grouping command) to be executed.

case echo in
    a) b;;
    (c) d;;
    (e|f) g;;
    (h) (i);;
    (j)
esac
