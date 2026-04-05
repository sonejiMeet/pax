@echo off

rem `build`          [fast compile time, no optimization]
rem `build release`  [with optimization]

echo:
for %%a in (%*) do set "%%~a=1"
if "%release%"=="1" set optimize=/O2        && echo [Release mode]
if "%debug%"=="1"   set debug=/D_DEBUG /MDd && echo [Debug mode]
if "%~1"=="" 								   echo [Default mode]

pushd src\

cl %debug% %optimize% /Z7 /w /EHsc /nologo /Zc:preprocessor /Zc:inline main.cpp logo.res /Fe:pax.exe /link /INCREMENTAL:NO /DYNAMICBASE:NO


: for .i file
rem cl %debug% %optimize% /EP /W3 /EHsc /nologo /Zc:preprocessor /Zc:inline main.cpp /link /INCREMENTAL:NO /DYNAMICBASE:NO


: detailed compilation time of each phase and components
rem cl /Bt+ /d2cgsummary /d1reportTime /W3 /EHsc /nologo /Zc:inline main.cpp /Fe:pax.exe /link /INCREMENTAL:NO

: breif compilation time of each phase
rem cl /Bt+ /W3 /EHsc /nologo /Zc:inline main.cpp /Fe:pax.exe /link /INCREMENTAL:NO


rem g++ -o pax1.exe -w main.cpp
rem clang++ -o pax1.exe -w main.cpp

popd