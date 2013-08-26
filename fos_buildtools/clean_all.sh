#!/bin/sh
echo "--> Cleaning"
find .. -name '*.res' -exec rm {} ';'
find .. -name '*.o' -exec rm {} ';'
find .. -name '*.ppu' -exec rm {} ';'
rm -rf full_build_log.txt 1>/dev/null
rm -rf ../../bin 1>/dev/null
rm -rf ../../fospkg 1>/dev/null
