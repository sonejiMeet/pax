# About Pax language
Pax is a strongly and statically typed, systems language with type inference and
order-independent struct and function declarations.

# About Pax compiler/transpiler
At present, Pax is still a transpiler (transpiles to C-like C++) it does not rely 
on C's compiler for error messages whatsoever (or atleast its the goal).

So it would be underselling to say its just a transpiler, because actually 
it does a complete analysis before generating any C code. If your Pax code
compiles, the generated C code is guranteed to be correct, with no errors 
from C compiler (otherwise its a bug in Pax transpiler that should be fixed).

Eventually, the aim is to shift from transpiling to a native backend.
For now, I wanted to focus on language design and frontend implementation.

# How to build
## On Windows (requires VS 2022)
```C
./build.bat
./tests.bat // check if all tests pass
```
## On Linux
```C
./build.sh
./tests.sh // check if all tests pass
```
# Usage
```C
.\bin\pax.exe // pax compiler exists in bin folder
```
```
Usage :  pax file.pax [option]
Option:
        -time,     show how long each step takes
        -profile,  enable memory tracer (outputs a text file)
        -lex,      print lexer tokens
        -verbose,  continue past errors, report all at once
        -mem,      windows memory leak check
        -debug,    debug
        -h,        print help
```
# Important note
For now the build system of Pax may not seem reliable. Main built-in modules like General.pax, String.pax and Sort.pax are inside modules/ folder. This folder is searched at the beginning for their existence, if it does not exit there then it falls back to local path. 

Also, the only way to compile Pax project is to pass a file with atleast main entry point (main function) and as long as it has relative paths to other pax module files it uses its fine. Best way is to just keep all the Pax files in single folder including main entry point.


# Memory tracing
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
## How to use memory tracer
1. Pass the **-profile** argument when compiling a pax file, see example below 
```bash
.\src\pax.exe main.pax -profile
```
2. Now it will output a `pool_trace.txt`
3. Run following to generate json file
```bash
python mem_tracing\main.py pool_trace.txt
```
4. Upload `pool_trace.json` to [Perfetto](https://ui.perfetto.dev/)
