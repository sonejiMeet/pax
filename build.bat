@echo off

pushd src\

cl /W3 /EHsc /nologo /Zc:inline main.cpp all.cpp /Fe:pax.exe /link /INCREMENTAL:NO
rem cl /Bt+ /Z7 /W3 /EHsc /nologo /Zc:inline main.cpp all.cpp /Fe:pax.exe /link /INCREMENTAL:NO

rem g++ -o pax.exe main.cpp all.cpp
popd