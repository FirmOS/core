#! /bin/sh
echo TO CLEAN
find .. -name '*.o' -exec echo {} ';'
find .. -name '*.ppu' -exec echo {} ';'
echo CLEAN
find .. -name '*.o' -exec rm {} ';'
find .. -name '*.ppu' -exec rm {} ';'
echo CLEANED
find .. -name '*.o' -exec echo {} ';'
find .. -name '*.ppu' -exec echo {} ';'
