# !/usr/bin/env bash

rm run_tests.exe
g++ -o run_tests.exe -O2 run_tests.c
./run_tests.exe