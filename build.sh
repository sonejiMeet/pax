#!/bin/bash

DEBUG_FLAG=""
if [ "$1" == "debug" ]; then
    DEBUG_FLAG="-D_DEBUG"
fi

cd src/

COMMAND="g++ $DEBUG_FLAG -O2 -w -o main main.cpp lexer.cpp parser.cpp tools.cpp code_manager.cpp c_converter.cpp"
echo $COMMAND

$COMMAND

cd ..