#!/bin/sh
# See [2.2 Quoting] and [2.2.2 Single-Quotes]
# The last symbol on the line starting with echo is a <tab>.
# There should be one argument to echo, with an embedded >newline>.

echo '| & ; <> () $ ` \ " 	
 * ? [ # ~ = %'
