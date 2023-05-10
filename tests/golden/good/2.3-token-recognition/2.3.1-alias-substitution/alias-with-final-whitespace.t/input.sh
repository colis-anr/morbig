# From POSIX 2018, Shell & Utilities, 2.3.1 Alias Substitution:

# If the value of the alias replacing the word ends in a <blank>, the
# shell shall check the next command word for alias substitution; this
# process shall continue until a word is found that is not a valid alias
# or an alias value does not end in a <blank>.

alias x='echo '
alias z='a '

         # output
x x      # echo
x y x    # y x
x x x    # echo echo
x x y x  # echo y x
x z x
