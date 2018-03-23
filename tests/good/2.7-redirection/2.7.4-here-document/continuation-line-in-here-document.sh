# 2.7.4: if the delimiter is not quoted then backslash behaves as in double
# quotes, that is <backslash><newline> indicates a line contunation.
if true
then
cat << EOF
first line
second line
third line with continuation\
EOF
forth and last line ((
EOF
else echo "baeh"
fi
