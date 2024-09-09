#!/usr/bin/env python2
import sys
import marshal
import dis
import argparse

if __name__ == '__main__':
    parser = argparse.ArgumentParser("Execute compiled pycode object")
    parser.add_argument('filename')
    parser.add_argument('-d', '--debug', action='store_true', help='Show code object')
    args = parser.parse_args()
    inf = open(args.filename, 'rb')
    code = marshal.load(inf)
    if args.debug:
        dis.disassemble(code)
        import pdb; pdb.set_trace()
    exec(code)
    inf.close()
