## About Pax language
Pax is a strongly and statically typed, systems language with type inference and
order-independent struct and function declarations.

## About Pax compiler/transpiler
At present, Pax is still a transpiler (transpiles to C-like C++) it does not rely 
on C's compiler for error messages whatsoever (or atleast its the goal).

So it would be underselling to say its just a transpiler, because actually 
it does a complete analysis before generating any C code. If your Pax code
compiles, the generated C code is guranteed to be correct, with no errors 
from C compiler (otherwise its a bug in Pax transpiler that should be fixed).

Eventually, the aim is to shift from transpiling to a native backend.
For now, I wanted to focus on language design and frontend implementation.

## Memory tracing
Pax uses Pool allocator (aka bump allocator), where a huge chunk of memory (ex. 128 KiB)
is allocated by the global malloc and data is pushed in contiguous form and pointer is 
bumped as the memory block gets filled. To debug allocator I had all the different 
phases of Pax compiler that allocate to the Pool print useful information about memory 
allocations, such as, type of allocation, how many bytes, how much extra padding is 
added, which line it is etc. see image below.

![output to terminal](https://github.com/user-attachments/assets/9aabd114-fbea-4f6b-aca6-367412d414ac)
---
This was useful but printing large text content directly to terminal is simply not convenient. 

So I decided to visualize it using [Perfetto](https://ui.perfetto.dev/) profiler, 
where we can conveniently upload a Json file which our converter in **mem_tracing/** folder generates. 
see below.

https://github.com/user-attachments/assets/5eb5fc7f-b7d5-4cc5-8596-2fdbb91050db

---
(This project is under development)

###### P.S. Language design is heavily inspired by Jonathan Blow's Jai programming language.
