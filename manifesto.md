# $\color{almond}\textit{fluent}$

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

### function effect handlers and context state mutation

effect handlers are commonly described as a generalization of exceptions, but
I think a nicer way to describe them is as a type-checkable mechanism to escape
function purity.

in more imperative and procedural languages, mutating state is not something
that the compiler is able to check. a common source of bugs in many languages
is code where the programmer must remember, without the compiler's help, to
do some sort of branch or stateful mutation after calling some procedure. think
`begin()` and `end()` types of functions, `init()` and `deinit()`, errors,
panics, null checks.

*fluent*'s solution is explicit effect handlers and context management. *fluent*
functions must explicitly declare any side effects or required effect handlers
(e.g. contexts), which allows *fluent* to perform effect type analysis and also
build language features around this.

here's an example of a zig function decl and the equivalent (goal) *fluent*:

```zig
const Allocator = @import("std").mem.Allocator;

fn dupe(ally: Allocator, bytes: []const u8) Allocator.Error![]u8 {
  // ...
}
```

```
// defines an effect interface which:
// - provides a context object
// - provides a realloc() function which provides memory management
// TODO unsure about this syntax
mem :: effect {
  context: *mut opaque,
  realloc: *Fn {T} {[]T, usize} slice T,
}

alloc :: fn { size: usize } mem []mut u8 ->
  mem.realloc &{} size;

dupe :: fn { data: []u8 } mem []u8 ->
  // `ref` ensures a variable is stack allocated and gives you a `*mut` to it
  cloned = ref (alloc data.len);
  memcpy cloned data; // memcpy will probably be a prelude function
  cloned
```

you could then do something along the lines of:

```
with my-allocator (dupe bytes)
```

these effects and contexts can be implicitly passed between functions, to
prevent the kinds of noise you see in Zig code. `with` blocks allow redefining
effects and handlers.

the generalizability of this system means that things like member functions
and closures can also be encoded. under the hood, overhead for this can just
compile down to passing pointers around to any contexts or handlers.
