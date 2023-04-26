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
    - [ ] split the code in sub-directory to isolate boot files
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
  - IR:
    - [ ] can i make thunks `Void` when a value is concrete?
    - [ ] can i use trees that grow or equivalent to make aliases `Void` after inlining?

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
  - [x] why can't let clause recursively refer to themselves?
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
  - [ ] the documented `import "math/bits:bits"` syntax doesn't seem to work?
  - [ ] the playground does not reject having multiple imports with the same alias
  - [ ] the reference claims that `len([1,2,3,...]` is `>= 3`, but the playground resolves it to `3`
  - [ ] the playground allows "_" as an alias, but it can't be used, which always results in an error
  - [ ] disjunction merges `s1 | s1` into `s1` even if their attributes differ, keeping the attributes of the left-most one
  - [ ] disjunction CREATES NEW ATTRIBUTES?!
```cue
// in
a: {
  a:1
  @foo(bar)
}
b:{
  a:2
  @foo(baz)
}
c: a | b

// out
a: {
  @foo(baz)
  a: 1
}
b: {
  @foo(bar)
  a: 2
}
c: {
  {
    @foo(baz)
    a: 1
  } |
  {
    @foo(bar)
    a: 2
  }
  @foo(bar)
}
```
  - [ ] the reference claims that default values are propagated through list indexing, but that doesn't seem to be true?
  - [ ] the reference doesn't document partial slices of the form `[:]`, `[a:]`, `[:a]`
  - [ ] the reference doesn't document slices at all actually?
