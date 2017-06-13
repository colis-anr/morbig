#!/bin/sh

# Specification 2.7.4 Here-Documents:
# If any part of word is quoted,
# the delimiter shall be formed by performing quote removal on word,
# and the here-document lines shall not be expanded.

if true
then cat <<'E'"O"\F
Here document not expanded, may be unbalanced ' " $(
EOF
fi
