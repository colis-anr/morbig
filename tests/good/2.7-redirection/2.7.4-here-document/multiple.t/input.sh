#!/bin/sh

# The here-document shall be treated as a single word that begins
# after the next <newline> and continues until there is a line
# containing only the delimiter and a <newline>, with no <blank>
# characters in between. Then the next here-document starts, if there
# is one.

cat > toJohn << EOF1 ; cat > toJane <<- EOF2
Hi John!
EOF1
    Hi Jane!
EOF2
