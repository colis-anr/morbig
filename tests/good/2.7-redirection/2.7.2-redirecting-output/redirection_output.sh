#!/bin/sh
# See [2.7.2 Redirecting Output]

echo >word
echo >|word

# redirect file descriptor 2
echo 2>word
echo 2>|word

# 3 is argument of echo, redirect stdout
echo 3 >word
echo 3 >|word

# redirect file descriptor 4
echo 4> word
echo 4>| word

# 5 is argument to echo, redirect stdout
echo 5 > word
echo 5 >| word
