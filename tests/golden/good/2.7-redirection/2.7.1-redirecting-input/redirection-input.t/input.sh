#!/bin/sh
# See [2.7.1 Redirecting Input]

# redirect stdin from word
echo <word

# redirect file descriptor 2 from word
echo 2<word

# 3 is argument to echo, redirect stdin from word
echo 3 <word

# redirect file descriptor 4 from word
echo 4< word

# 5 is argument to echo, redirect stdin from word
echo 5 < word
