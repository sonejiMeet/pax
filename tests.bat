@echo off
setlocal enabledelayedexpansion

cl /nologo -O2 run_tests.c /Fe:run_tests.exe
run_tests.exe
