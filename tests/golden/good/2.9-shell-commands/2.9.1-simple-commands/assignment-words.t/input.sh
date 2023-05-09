#!/bin/sh

CC=gcc make
make CC=cc
ln -s /bin/ls "X=1"
"./X"=1 echo

# On line 1, the word CC=gcc is recognized as a word assignment of gcc
# to CC because CC is a valid name for a variable, and because CC=gcc
# is written just before the command name of the simple command
# make. On line 2, the word CC=cc is not promoted to a word assignment
# because it appears after the command name of a simple command. On
# line 4, since "./X" is not a valid name for a shell variable, the
# word "./X=1" is not promoted to a word assignment and is interpreted
# as the command name of a simple command.
