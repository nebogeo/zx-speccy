#!/bin/sh -e
# Usage: ./makez80 hello

#z80asm $@.asm ; bin2tap/bin2tap a.bin ; xspect a.tap

zxbasic/zxbasm.py -TaB $@.asm ; fuse --speed 89 $@.tzx
