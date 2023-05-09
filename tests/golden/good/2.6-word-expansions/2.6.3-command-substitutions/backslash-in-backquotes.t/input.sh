#!/bin/sh

# derived from the postinst of monotone-server.

# Posix 2.6.3: Within the backquoted style of command substitution,
# <backslash> shall retain its literal meaning, except when followed
# by: '$', '`', or <backslash>.

# Hence, executing this should print a " on stdout

echo `echo "\\""`
