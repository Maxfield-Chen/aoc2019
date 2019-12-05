#!/bin/bash

stack new $1 simple
#cp stack.yaml $1/stack.yaml
#cp template.cabal $1/$1.cabal
mkdir $1/data
touch $1/data/part1.txt
touch $1/data/part2.txt

# Fix incorrect executable name in template.cabal
sed -i "s/aoc2018/$1/g" $1/$1.cabal
