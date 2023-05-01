# HINT: A Haskell interpreter for CUE

HINT is a parser / interpreter library for the [CUE language](https://cuelang.org/). While it aims at being fully compliant with the language reference, it is a hobby project and is therefore provided "as-is".

## Language features checklist

- [x] basic numeric and string expressions
- [x] constraints
  - [x] type constraints (`number`, `string`)
  - [x] unary constraints (`>3`, `!= null`)
- [ ] lists
  - [x] simple elements
  - [ ] list comprehensions
- [ ] structs
  - [ ] closed structs reject new fields
  - [x] regular fields
  - [ ] constraints (missing alias reference)
  - [ ] string fields
  - [ ] embedded expressions
- [x] lazy field references
- [x] alias references
- [x] field selection
- [x] list indexing (and slices)
- [ ] cycle detection
- [ ] batteries included
  - [x] built-in functions
  - [ ] other standard packages

## Project TODO list

- [ ] add location information to AST by making it a HKD
- [ ] add location information the IR?
- [ ] add proper error handling to the translation layer instead of panic
- [ ] add proper error handling to the evaluation layer:
  - [ ] distinguish between fatal errors (such as structural cycles), bottom expressions (such as `> true`), and postponed evaluation (such as `a: {}, b: a.r`); this will require changing some of the evaluation logic to distinguish between `number > 0` (bottom) and `a: number, b: a > 0` (postponed)
  - [ ] decorate values with origin marker?
- [ ] add string fields in struct evaluation
  - [ ] attempt to resolve them when resolving to WHNF, merge on success, postpone on failure
  - [ ] during NF evaluation, loop on resolving them until all of them are resolved or we fail to make progress (postpone error)
- [ ] resolve embeddings during WHNF eval
  - [ ] eval clauses to WHNF
  - [ ] perform ad-hoc field reference substitution with thunks
  - [ ] detect embeddings type conflicts during WHNF phase?
- [ ] add JSON and YAML conversion functions
- [ ] add generic `fromCUE` function (relying on JSON?)
- [ ] add proper license to the project

## CUE issues

Inconsistencies in the reference, weird behaviours in the playground... Feedback to submit to the CUE community, and questions to ask about the language.

- [x] splices aren't documented in the reference (https://github.com/cue-lang/cue/discussions/2368, https://github.com/cue-lang/cue/issues/772)
- [x] the reference implementation's lexer accepts tokens that are not mentioned in the reference, such as the power operator `^` and the arrow operator `<-` (https://github.com/cue-lang/cue/discussions/2385)
- [x] why does CUE reject newlines in string interpolations? (https://github.com/cue-lang/cue/discussions/2384)
- [x] ambiguous grammar: why does the reference insists on a distinction between `QualifiedIdentifier` and `Selector`? (https://github.com/cue-lang/cue/discussions/2386)
- attributes:
  - [x] the playground discards attributes before the package name when a package name is present (https://github.com/cue-lang/cue/issues/2371)
  - [x] the playground allows attributes between package name and imports (https://github.com/cue-lang/cue/issues/2371)
  - [x] disjunction deduplicates `s1 | s2` into `s1` if they are similar struct *even if their attributes differ*, keeping the attributes of the left-most one (https://github.com/cue-lang/cue/issues/2372)
  - [x] disjunction can create new attributes (https://github.com/cue-lang/cue/issues/2373)
- imports
  - [ ] the documented `import "math/bits:bits"` syntax doesn't seem to work?
  - [ ] the playground does not reject having multiple imports with the same alias
- mandated utilisation
  - [x] the reference doesn't document the fact that aliases must be used (https://github.com/cue-lang/cue/issues/2381)
  - [ ] the reference doesn't document the fact that imports must be used
- aliases
  - [x] the playground will emit erroneously cue expressions by eagerly inlining aliases (https://github.com/cue-lang/cue/issues/2378)
  - [x] aliasing to a constraint leads to incorrect output (https://github.com/cue-lang/cue/issues/2374)
  - [ ] aliases will be automatically renamed in the output?
  - [x] the reference claims that an `Embedding` can be an `AliasExpr`, but the playground rejects such aliases (https://github.com/cue-lang/cue/issues/2377)
  - [x] the reference claims that aliases to optional fields are only visible within the field, but the playground disagrees (https://github.com/cue-lang/cue/issues/2379)
- [x] what is the meaning of non-string constraints? why don't they result in an error? (https://github.com/cue-lang/cue/discussions/2383)
- [x] the reference claims that `len([1,2,3,...]` is `>= 3`, but the playground resolves it to `3` (https://github.com/cue-lang/cue/issues/2376)
- [x] the playground allows "_" as an alias, but it can't be used, which always results in an error (https://github.com/cue-lang/cue/issues/2381)
- [x] the reference claims that default values are propagated through list indexing, but that doesn't seem to be true? (https://github.com/cue-lang/cue/issues/2382)
- [x] `>(<=3)` and other unary conditions on bounds are not documented (https://github.com/cue-lang/cue/issues/2380)
