#!/bin/sh
# See [2.7.2 Redirecting Output]

echo >word
echo >|word

echo 2>word
echo 2>|word

echo 3 >word
echo 3 >|word

echo 4> word
echo 4>| word

echo 5 > word
echo 5 >| word
