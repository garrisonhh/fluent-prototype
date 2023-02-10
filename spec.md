# spec

at this stage in fluent's development I have no intention of trying to keep this
updated with the exact state of fluent currently, the point of this document is
to write down language features that I want to crystallize, regardless of what
stage of implementation fluent is in.

## syntax

all fluent syntax gets translated to S-expressions before executing. you can
think of operators `<lhs> op <rhs>` as sugar for `(op <lhs> <rhs>)`. since
fluent's syntax is almost exclusively expression-based, you can think of every
piece of syntax as being rewritten in this way.

```
# fluent files are a series of declarations
name :: 0

# expressions aim to emulate lambda calculus + math
x :: 1 + 2 * (3 + 4)

# equivalent to `(flop a) + (flip b)`
y :: flop a + flip b

# fluent is a strictly, strongly, explicitly typed language. the syntax and
# semantics are designed to allow you to omit types wherever the execution
# concerns remain clear (but you can always be explicit with `as`)
z :: as i64 34

# fluent has functions, including higher-order functions
add ::
  as (Fn &[i64, i64] i64)
    |a, b| a + b

sum ::
  as (Fn &[Slice i64] i64)
    |xs| reduce `+ xs

# sugar for lambda + cast exists
sweet_sum :: fn {xs: Slice i64} -> i64 =
  reduce `+ xs

# fluent has control flow and recursion
fibonacci :: fn {n: i64} -> i64 =
  if n == 0 then 0
  else if n == 1 then 1
  else fibonacci (n - 1) + fibonacci (n - 2)
```

## ssa call convention

*src.backend.canon.Repr is the base truth for this, always!*

fluent's bytecode vm aims to act like a simplified 64-bit cpu, with some
high-level superpowers for things like executing types. there are a number of
64-bit registers, and memory that is available to manipulate.

fluent functions take some number of parameters and return a single value. there
is also an implicit context parameter, which contains anything necessary for
effect handling.

- the context variable acts like an invisible first parameter
- collections, arrays, and functions are always referenced, never by value
- parameters are passed on the registers 1..=N (where N = #params)
  - reference reprs are passed by pointer
- return values have two conventions
  - for reference reprs, the caller must pass a pointer to available memory in
    register 0
  - for value reprs, the callee must return the value through register 0