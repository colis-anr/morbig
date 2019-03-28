#!/bin/sh

# this is in fact allowed by the grammar: a compound_list is not required
# to be terminated by a separator. However, this seems to be bug in the
# posix shell grammar (!)

until a do b; done
