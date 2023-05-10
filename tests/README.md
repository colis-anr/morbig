# Morbig's Test Suite

## Golden tests

Those are found in the `tests/golden/` directory.

### Directories

This test suite contains four top-level directories corresponding to four
categories of tests, namely:

- `good/`: these adhere to the POSIX standard, and should be accepted.
- `bad/`: these do not adhere to the standard, and should be rejected.
- `unspecified/`: cases where the standard says that the behavior is unspecified
- `unknown/`: cases where we could not figure out whether there are good, bad,
  or unspecified.

A test is a directory whose name finishes in `.t`. Such tests can be nested as
subdirectories of the four top-level ones at an arbitrary depth. Test
directories contain an input file `input.sh`. Good tests contain an additional
expectations file `expected.json`. Finally, tests may contain an `open` file
that specifies that the test in question has an open issue attached to it; such
tests are expected to fail.

### How to add tests

Tests are structured by the section of the policy concerned, whenever possible.
Please add a comment at the beginning of the test file, explaining what is being
tested.
