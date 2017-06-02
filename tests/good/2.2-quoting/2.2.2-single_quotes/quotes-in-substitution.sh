#!/bin/sh

# Specification 2.2.2 :
# Enclosing characters in single-quotes ( '' ) shall preserve the
# literal value of each character within the single-quotes. A
# single-quote cannot occur within single-quotes.

# Hence, the following is well balanced since the first ) is protected
# by the single quotes:

echo $(echo ')')
