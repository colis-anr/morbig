#!/bin/sh
# See [2.7.6 Duplicating an Output File Descriptor]

# open word for reading and writing 
echo <>word

# open word for reading and writing on file descriptor 2
echo 2<>word

# open word for reading and writing, 4 is argument to echo
echo 4 <>word

# open word for reading and writring from file descriptor 5
echo 5<> word

# open word for reading and writing, 6 is argument to echo
echo 6 <> word
