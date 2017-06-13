#!/bin/sh

# This is a classical bashism.
# Accordingto POSIX, this executes "echo" in the background, and sends
# its stdout to foo (as if it were "& >")

echo &> foo
