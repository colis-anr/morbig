#!/bin/sh
# See [2.7.3 Appending Redirecting Output]

# redirect-append stdout to word
echo >>word

# redirect-append file descriptor 2 to word
echo 2>>word

# 3 is argument to echo, redirect-append stdout to word 
echo 3 >>word

# redirect-append file descriptor 4 to word
echo 4>> word

# 5 is argument to echo, redirect-append stdout to word
echo 5 >> word
