#!/bin/sh

# First level
echo "\""

# Second level
echo " $(echo `echo "\\""` )"

# Is it possible to have a third level of nesting?
