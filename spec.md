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

```fluent
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
