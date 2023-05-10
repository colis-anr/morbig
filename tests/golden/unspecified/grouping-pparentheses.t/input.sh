#!/bin/sh
# See [2.9.4 Compound Commands], paragraph [Grouping Commands]:

# If a character sequence beginning with "((" would be parsed by the
# shell as an arithmetic expansion if preceded by a '$', shells which
# implement an extension whereby "((expression))" is evaluated as an
# arithmetic expression may treat the "((" as introducing as an
# arithmetic evaluation instead of a grouping command. A conforming
# application shall ensure that it separates the two leading '('
# characters with white space to prevent the shell from performing an
# arithmetic evaluation.

((a))
