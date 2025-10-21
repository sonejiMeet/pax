@echo off

pushd src\
cl /Z7 /W3 /EHsc /nologo /JMC /Zc:inline main.cpp lexer.cpp parser.cpp tools.cpp code_manager.cpp c_converter.cpp interp.cpp /link /INCREMENTAL:NO
popd