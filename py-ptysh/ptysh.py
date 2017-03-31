#!/usr/bin/env python3
import sys
import pty

print("corny ptysh TEST", flush=True)

ret = pty.spawn('/bin/sh')

assert ret < 2**16
killsig = ret & 0xff
if killsig != 0:
    print("abnormal termination")
retcode = (ret & 0xff00) >> 8

print("exited {}".format(retcode), file=sys.stderr)
sys.exit(retcode)
