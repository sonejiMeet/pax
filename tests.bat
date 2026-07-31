@echo off
setlocal enabledelayedexpansion

cl /nologo run_tests.c /Fe:run_tests.exe
run_tests.exe
