#!/bin/bash

DEBUG_FLAG=""
OPTIMIZE_FLAG=""

if [ "$1" == "release" ]; then
    OPTIMIZE_FLAG="-O2"
    echo "[Release mode]"
elif [ "$1" == "debug" ]; then
    DEBUG_FLAG="-D_DEBUG"
    echo "[Debug mode]"
else
    echo "[Default mode]"
fi

mkdir bin
cd src/

COMMAND="g++ -ggdb $DEBUG_FLAG $OPTIMIZE_FLAG -w -o ../bin/pax.exe main.cpp"
echo $COMMAND

$COMMAND

cd ..

echo
echo "Output: ./bin/pax.exe"
