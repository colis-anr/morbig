# if any part of the delimite is quoted, then the backslash in the here
# document behaves as the backslash inside double quotes: it shall
# retain its special meaning as an escape character only when followed
# by one of the following characters when considered special:
# $   `   "   \   <newline>

cat << "EOF1"
\a\$\\
EOF1
cat << EOF2
\a\$\\
EOF2
