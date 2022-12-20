# $\color{almond}\textit{fluent}\color{white}\textup{lang}$

## compilation steps

1. lexing + syntax checking
2. parsing + type checking
3. dynamic code evaluation with code as data
    - compiling programs that compile programs
4. static compilation of final code output to qbe/llvm

combining compile time execution (CTE) with DSL-focused functional programming
while staying as close as practical to the hardware is the goal of *fluent*lang.

*fluent* binaries will have all of the best micromanaged performance traits of
compiled, static procedural languages, while programmers enjoy the niceness of
homoiconicity and heavily functional code paired with a compiler that deeply
understands the code.

## ideas

### more ML-like syntax

lispy semantics with whitespace awareness. the important property of syntax for
Fluent is that it preserves a strong relationship with the AST, so that code
using Fluent's homoiconic features retains low cognitive overhead

### distinct and flow types

when I'm writing code of any significant size (like the fluent compiler) I'm
always thinking about the flow of data from start to finish. there are two
issues I see crop up that I think could be fixed with type system features:

```
(def Shader Type (Distinct U32))
```

- 'stringly typed programming'. this refers to a specific problem in languages
  like JS, but applies to more than just strings. take `int` or bitfields in
  C/Zig as an example.
  - distinct types allow you to let the compiler verify checks for you. for
    example, you could define a `distinct i8` that represents a value that is
    -1, 0, or 1 (commonly used as representing comparison in c). you can now
    refer to this type with a name like `comparison`, and it is now
    significantly harder for you to accidentally introduce bugs in low-level
    sorting code.
  - distinct types use the same backed data and operations, but disallows
    implicit casting
  - without distinct types, generalizing your types (especially primitives)
    always results in less type-safe code. creating more general code should not
    be a footgun.
  - distinct types should allow their own operation definitions

```
(def Fluent Type
  (Flow [
    (Input String)
    (Ast Expr)
    (Output (List Op))]))

// types are then referred to as (Fluent Input), (Fluent Ast) etc.
```

- misordering or missing steps of a process. this is a super common source of
  bugs. think of long math operations with many steps, all on `float` or `int`.
  or simple tasks like filling an `ArrayList`.
  - flow types allow you to create multiple distinct types all at once. on top
    of the normal properties of distinct types, flow types reflect the intended
    lifetime of the underlying semantic data.

### handle types

*this may or may not be a good addition to `distinct` type features*

```
(def table (Array (Tuple String Int)) [("hi" 0) ("bye" 2)])

(def table-row Type (Handle table))
```