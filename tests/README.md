Morbig's Test Suite
===================

Directories
-----------

- `good/`: these adhere to the POSIX standard, and should be accepted.

- `bad/`: these do not adhere to the standard, and should be rejected.

- `unspecified/`: cases where the standard says that the behavior is unspecified

- `unknown/`: cases where we could not figure out whether there are good, bad,
  or unspecified.

Good tests contain an `.expected` result, to which the output of Morbig will be
compared. A good test is said to yield un `unexpected` result if Morbig succeeds
but the output is not what was expected. A good test is said to fail if Morbig
fails.

Good and bad tests may contain an `.open` file that specifies that the test in
question is “open”, that is there is an open issue corresponding to it. Those
tests may fail or succeed without making the whole test suite fail or succeed.

How to add tests
----------------

Tests are structured by the section of the policy concerned, whenever possible.
Please add a comment at the beginning of the test file, explaining what is being
tested.
