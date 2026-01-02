@echo off

pushd src\
cl /Zi /W3 /EHsc /nologo /Zc:inline main.cpp lexer.cpp parser.cpp tools.cpp code_manager.cpp c_converter.cpp interp.cpp /link /INCREMENTAL:NO
popd