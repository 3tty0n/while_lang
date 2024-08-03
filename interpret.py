#!/usr/bin/env python2
import sys
import marshal
import dis

if __name__ == '__main__':
    inf = open(sys.argv[1], 'rb')
    code = marshal.load(inf)
    dis.disassemble(code)
    exec(code)
    inf.close()
