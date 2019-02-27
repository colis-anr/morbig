#!/usr/bin/env sh

test () {
  case $1 in
       [abc]) echo 'a, b, or c';;
       [---]) echo 'hyphen';;
       a[a-]) echo 'a and (an a or an hypen)';;
       a[b!]) echo 'a then b or hat';;
       [[=e=]]) echo 'equivalent to e';;
       [[.].]]) echo 'right bracket';;
       [[.!.]]) echo 'hat';;
       [2-]) echo 'digit >= 2';;
       [^0]) echo 'this is not "not zero"';;
       [!0]) echo 'not zero';;
       [[:digit:]]) echo 'zero';;
       *) echo "default";;
  esac
}

test ']'
test '!'
test '-'
test 'a-'
test '0'
test '1'
test '2'
test 'e'
test 'a'
test 'a!'
test 'ab'
test 'something else'

