# HINT: A Haskell interpreter for CUE

HINT is a parser / interpreter for the [CUE language](https://cuelang.org/). This is a WIP. It doesn't work. Do not use.

TODO:
  - Project
    - [x] create custom prelude
    - [x] add stylish-haskell configuration
    - [ ] add hlint configuration?
    - [x] cleanup project config
    - [ ] write documentation
    - [ ] decide on a license
  - Binary
    - [ ] decide on a name
    - [ ] decide on a list of operations
    - [ ] does this project even need a binary?
  - Tests
    - [ ] split tests in files
    - [ ] make optional suite that compares expression evaluation to CUE's?
    - [ ] benchmarking?
  - AST
    - [ ] carry location throughout the AST
  - Evaluation
    - [ ] everything

Questions to ask to the CUE community:
  - [ ] the go implementation accepts tokens that are not mentioned in the documentation, such as the power operator `^` and the arrow operator `<-`; are those only supported for the sake of attributes?
  - [ ] ask about newlines in string interpolations
  - [ ] ask about clarifications on default values:
    - [ ] do they unify for arithmetic operations, or just at the end when computing the result?
    - [ ] do boolean operations distribute like arithmetic operations do?
  - [ ] the grammar seems to be technically ambiguous, does it matter?
    - [ ] `foo.bar` is either a qualified identifier or a selector
    - [ ] we can't distinguish between attributes before or after a missing package name
