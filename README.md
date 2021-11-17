# HINT: A Haskell interpreter for CUE

HINT is a parser / interpreter for the [CUE language](https://cuelang.org/). This is a WIP. It doesn't work. Do not use.

TODO:
  - Project
    - [ ] create custom prelude
    - [ ] add stylish-haskell configuration
    - [ ] add hlint configuration?
    - [ ] cleanup project config
    - [ ] write documentation
    - [ ] decide on a license
  - Parser
    - [ ] implement labelName
    - [ ] fix field parsing
    - [x] add representation for comprehensions
    - [x] fix literals parsing
    - [ ] harmonize errors
    - [ ] generalize to any string type?
    - [ ] differentiate between "name" and "identifier" to avoid `#` in places such as package name?
  - Printer
    - [x] implement interpolation
    - [ ] implement fields / values
    - [x] implement comprehensions
    - [x] properly escape string values
    - [ ] cleanup block printer
  - Evaluation
    - [ ] everything
  - Binary
    - [ ] decide on a name
    - [ ] decide on a list of operations
  - Tests
    - [x] merge tests suite
    - [ ] reduce the size of arbitrary literals (rewrite to functions instead of instances)
    - [ ] debug remaining quickcheck issues
    - add test cases for known edge cases
      - [ ] `_|_` being interpreted as `_ | _`
      - [ ] identifier `#foo` being interpreted as a string literal
      - [ ] `foo.bar` which can be interpreted either as a selector or as a qualified identifier
      - [ ] `123.foo` being a parse error for floats rather than an invalid selector
    - [ ] find a real file for simple e2e tests
    - [ ] benchmarking?
