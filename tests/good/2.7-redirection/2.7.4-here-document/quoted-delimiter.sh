#!/bin/sh

# Specification 2.7.4 Here-Documents:
# If any part of word is quoted,
# the delimiter shall be formed by performing quote removal on word.

if true
then cat <<'E'"O"\F
contents of the here document
EOF
fi
