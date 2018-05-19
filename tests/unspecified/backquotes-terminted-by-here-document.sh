# Section 2.6.3 Command Substitution:
# The search for the matching backquote shall be satisfied by the first
# unquoted non-escaped backquote; during this search, if a non-escaped
# backquote is encountered within a shell comment, a here-document, an
# embedded command substitution of the $(command) form, or a quoted
# string, undefined results occur.

x=`cat << EOF
a`
echo $x
