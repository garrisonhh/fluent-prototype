# todos

this isn't for stuff I can throw in the code, more sweeping ideas I might want to do eventually

- homoiconicity: in order to implement type annotations, quote, and quasi-quote naturally I need to start compiling FlValues which represent the AST. this means adding a step between parsing and sema where I translate the textual AST (Exprs) to Fluent
    - also implies that I need to meld FlValue and FlType. having thought this through, I think I have very little to do here -- FlType should use FlValue.Enum as its enum, and instead of being the canonical representation of a type, in dynamic land FlValues simply store their type data and therefore the FlType can be extracted by doing a tree walk of the FlValue. (this is pretty cool and compositional)
    - I am vaguely worried about what this means for implementing scoping, something to think about
    - something else to think about is how i'm going to differentiate between static and dynamic execution. in my head I'm imagining using quote/quasiquote as the differentiation, where every fluent program is a dynamic program that returns the AST of a static program. this is not only elegant from a compiler implementation perspective but also takes advantage of lisp's cultural standards
        - a program which returns a program also means that things like build systems and preprocessing steps will compose incredibly organically!
- create an `context/` + `context.zig` pair, extracting the behavior encompassed by FlFile to something more organized and scaleable to a large project structure