#!/bin/sh

# Specification 2.2.1:

# A <backslash> that is not quoted shall preserve the literal value of
# the following character.

x=$(echo pi \) pa pu)
echo $x
