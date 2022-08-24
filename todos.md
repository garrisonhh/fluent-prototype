# todos

this isn't for stuff I can throw in the code, more sweeping ideas I might want to do eventually

- homoiconicity: in order to implement type annotations, quote, and quasi-quote naturally I need to start compiling FlValues which represent the AST. this means adding a step between parsing and sema where I translate the textual AST (Exprs) to Fluent
    - also implies that I need to meld FlValue and FlType. having thought this through, I think I have very little to do here -- FlType should use FlValue.Enum as its enum, and instead of being the canonical representation of a type, in dynamic land FlValues simply store their type data and therefore the FlType can be extracted by doing a tree walk of the FlValue. (this is pretty cool and compositional)
    - I am vaguely worried about what this means for
- create an `context/` + `context.zig` pair, extracting the behavior encompassed by FlFile to something more organized and scaleable to a large project structure