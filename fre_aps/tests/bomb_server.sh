#!/bin/sh
COUNT=1

while [ $COUNT -gt 0 ]; do
	echo BOMB Iteration count: $COUNT
	lib/i386-darwin/CON_apstest CLIENT 100
	echo $?
	let COUNT=COUNT+1
done 
