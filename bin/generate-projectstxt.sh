#!/bin/sh

# Assumption: Run within the quicklisp-projects.git:/projects repository

 grep -r '.' ./* | grep source.txt | sed -e 's/\.\/\(.*\)\/source.txt:\(.*\)/\1 \2/g' > ../projects.txt
