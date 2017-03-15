#!/usr/bin/env python3
import sys
import pty

print("ptysh", file=sys.stderr, flush=True)

print("hello", flush=True)

x = input()
print("you said {}".format(x), flush=True)

ret = pty.spawn('/bin/sh')

assert ret < 2**16
killsig = ret & 0xff
if killsig != 0:
    print("abnormal termination", file=sys.stderr)
retcode = (ret & 0xff00) >> 8

print("exited {}".format(retcode), file=sys.stderr)
sys.exit(retcode)
