#!/bin/bash

runhaskell StackGrep.hs exemplo.txt < exemplo_input.txt > resultado.txt

DIFF=$(diff resultado.txt exemplo_output.txt) 
if [ "$DIFF" != "" ] 
then
    echo "Failed tests!"
else
	echo "Passed tests!"
fi