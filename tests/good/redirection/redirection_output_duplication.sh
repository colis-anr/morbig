#!/bin/sh
# See [2.7.6 Duplicating an Output File Descriptor]

echo &>word

echo 2&>word
echo 4 &>word
echo 5&> word
echo 6 &> word

echo 7&>2
echo 8 &>-
echo 9&> $(echo 2)
echo 2 &> $((3 + 1))
