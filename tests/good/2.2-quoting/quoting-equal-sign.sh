#!/bin/sh

# According to POSIX 2018, rule 7.b of 2.10.2 Shell Grammar Rules
# The following sentences do not start with an assignmnent word.
a'='b
a"="b
a$(=)b
a`=`b
a$((=))b
# The following sentence does.
a=b
