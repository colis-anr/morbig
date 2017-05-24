#!/bin/sh

for i in a b; do echo $i; done
ls for i in a b

# On line 1, the words for, in, do, done are recognized as reserved
# words. On line 2, they are not recognized as such since they appear
# in position of command arguments for the command ls.
