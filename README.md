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
    - [x] implement labelName
    - [x] fix field parsing
    - [x] add representation for comprehensions
    - [x] fix literals parsing
    - [x] simplify unary operators
    - [ ] harmonize errors
    - [ ] generalize to any string type?
    - [x] differentiate between "name" and "identifier" to avoid `#` in places such as package name
  - Printer
    - [x] implement interpolation
    - [x] implement fields
    - [x] implement comprehensions
    - [x] properly escape string values
    - [x] cleanup block printer
  - Evaluation
    - [ ] everything
  - Binary
    - [ ] decide on a name
    - [ ] decide on a list of operations
  - Tests
    - [x] merge tests suite
    - [x] implement shrink everywhere
    - [x] debug remaining quickcheck issues
    - [x] fix missing arbitrary declarations
    - add test cases for known edge cases
      - [x] `_|_` being interpreted as `_ | _`
      - [x] identifier `#foo` being interpreted as a string literal
      - [x] `foo.bar` which can be interpreted either as a selector or as a qualified identifier
      - [ ] `123.foo` being a parse error for floats rather than an invalid selector
    - [ ] find a real file for simple e2e tests
    - [ ] make optonial suite that compares expression evaluation to CUE's?
    - [ ] benchmarking?

Questions to ask to the CUE community:
  - [ ] the go implementation accepts tokens that are not mentioned in the documentation, such as the power operator `^` and the arrow operator `<-`; are those only supported for the sake of attributes?
  - [ ] ask for clarification of `foo.bar` as qualified identifier vs selector
  - [ ] ask about newlines in string interpolations
  - [ ] ask about clarifications on default values:
    - [ ] do they unify for arithmetic operations, or just at the end when computing the result?
    - [ ] do boolean operations distribute like arithmetic operations do?
