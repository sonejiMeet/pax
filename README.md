Pax is a strongly and statically typed, imperative systems programming language.

Although Pax is still a transpiler (transpiles to C-like C++) it does not rely 
on C's compiler for error messages whatsoever (or atleast its the goal).

So it would be underselling to say its just a transpiler, because actually 
it does a complete analysis before generating any C code. If your Pax code
compiles, the generated C code is guranteed to be correct, with no errors 
from C compiler (otherwise its a bug in Pax transpiler that should be fixed).

For future the aim is ofcourse to shift from transpiling to native backend.
I wanted to focus on language design and frontend implementation.

P.S. Language design is heavily inspired by Jonathan Blow's Jai programming language.
