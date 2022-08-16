# $\color{almond}\textit{fluent}\color{white}\textup{lang}$

## compilation steps

1. lexing + syntax checking
2. parsing + type checking
3. dynamic code evaluation with code as data
    - compiling programs that compile programs
4. static compilation of final code output to C

combining compile time execution (CTE) with DSL-focused functional programming while staying as close as practical to the hardware is the goal of *fluent*lang.

*fluent* binaries will have all of the best micromanaged performance traits of compiled, static procedural languages, while programmers enjoy the niceness of homoiconicity and heavily functional code paired with a compiler that deeply understands the code.

## syntax

lispy semantics with whitespace awareness:

```
// this:
print (* (reduce + 0 [1 2 3]) 2)

// is the same as this:
print
  * (reduce + 0 [1 2 3]) 2

// is the same as this:
print
  *
    reduce + 0 [1 2 3]
    2
```