# 2.2.3 Double-Quotes:
# The <backslash> shall retain its special meaning as an escape character
# (see Escape Character (Backslash)) only when followed by one of the
# following characters when considered special:
# $   `   "   \   <newline>

# Hence: unquoting on the introductory occurrence of the delimiter must
# remove the backslash, here.

if true
then cat <<"E\$F"
hello
E$F
fi
