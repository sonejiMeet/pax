#!/bin/bash
g++ -O2 -w -o main main.cpp lexer.cpp parser.cpp tools.cpp code_manager.cpp c_converter.cpp
echo "main generated"