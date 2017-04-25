#!/bin/sh

rm -f cpp_defs.o
gcc -Wall -Wextra -Werror -fno-strict-aliasing -c cpp_defs.c -o cpp_defs.o
ar crv libcppdefs.a cpp_defs.o
cp -i libcppdefs.a ./target/debug/libcppdefs.a
