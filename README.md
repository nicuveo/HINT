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
    - [ ] can i make thunks `Void` when a value is concrete?

Questions to ask to the CUE community:
  - [ ] the go implementation accepts tokens that are not mentioned in the documentation, such as the power operator `^` and the arrow operator `<-`; are those only supported for the sake of attributes?
  - [ ] ask about newlines in string interpolations
  - [ ] the grammar seems to be technically ambiguous, does it matter?
    - [ ] `foo.bar` is either a qualified identifier or a selector
    - [ ] we can't distinguish between attributes before or after a missing package name
  - [ ] playground allows attributes on imports?
  - [ ] playground discards attributes before package name
  - [ ] why must aliases be used? the reference doesn't mention it
  - [ ] the reference doesn't document reference renaming
  - [ ] why can't let clause recursively refer to themselves?
  - [ ] the reference claims that an embed can be an aliased expression, but the playground rejects it?
  - [ ] the playground treats identifiers in comprehensions as fields, not aliases? is it because they don't need to be used?
  - [ ] the reference claims that aliases to optional fields are only visible within the field, but the playground disagrees
  - [ ] aliases get replicated wrong in the output:
```cue
// in
a: {
  x = b: number
  d: {
    b: 1
    c: x & a.z
  }
}

// out
a: {
  b: number
  d: {
    b: 1
    c: b & a.z // wrong b
  }
}
```
  - [ ] alias to a constraint does weird stuff ("X"?!):
```cue
// in
r = [!=null]: {a: r}
a: {}

// out
a: {
  a: X
}
```
  - [ ] the playground doesn't reject non-string pattern constraints
