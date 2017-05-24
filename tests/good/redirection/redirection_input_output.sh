#!/bin/sh
# See [2.7.6 Duplicating an Output File Descriptor]

echo <>word

echo 2<>word
echo 4 <>word
echo 5<> word
echo 6 <> word
